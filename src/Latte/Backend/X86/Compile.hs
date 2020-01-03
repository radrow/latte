{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
module Latte.Backend.X86.Compile where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Control.Monad
import Control.Monad.RWS.Lazy hiding ((<>))
import Control.Monad.State
import Prelude hiding ((<>), or, and)

import qualified Latte.Frontend.IR as IR
import Latte.Backend.X86.X86 as X86
import Latte.Backend.X86.PeepHole

type VarMap = M.Map IR.VarId Operand

data Env = Env
  { _envVarMap :: VarMap
  }
data St = St
  { _stSupply :: Int
  , _stStrings :: S.Set String
  }

type Compiler = RWS Env [Instr] St

makeLensesWith abbreviatedFields ''Env
makeLensesWith abbreviatedFields ''St

getLoc :: IR.VarId -> Compiler Operand
getLoc v = views varMap (M.! v)

nextSup :: Compiler Int
nextSup = do
  i <- use supply
  modify $ over supply (+1)
  return i

makeLabel :: String -> Compiler String
makeLabel s = do
  i <- nextSup
  let l = s ++ show i
  label l
  return l

makeStrName :: String -> String
makeStrName s = "local_str_" ++ s

requestStr :: String -> Compiler ()
requestStr s = modify $ over strings (S.insert s)

cConst :: IR.Const -> Operand -> Compiler ()
cConst ic o = case ic of
  IR.CVar c | isMem o -> do
    cloc <- getLoc c
    mov cloc edx
    mov edx o
  IR.CVar c -> do
    cloc <- getLoc c
    mov cloc o
  IR.CInt i -> do
    mov (OConst i) o
  IR.CStr s -> do
    requestStr s
    if isReg o
      then mov (OVar $ makeStrName s) o
      else do
      mov (OVar $ makeStrName s) edx
      mov edx o

cNOp :: IR.NumOp -> Operand -> Operand -> Compiler ()
cNOp = \case
  IR.Add -> add
  IR.Sub -> sub
  IR.Mul -> imul
  IR.Div -> idiv
  IR.And -> and
  IR.Or  -> or

cROp :: IR.RelOp -> Operand -> Operand -> Compiler ()
cROp = \case
  IR.Eq  -> \l r -> do
    cmp l r
    sete al
    test al al

cROpJmp :: IR.RelOp -> String -> String -> Compiler ()
cROpJmp o ltrue lfalse = do
  let j = case o of
        IR.Eq  -> je
        IR.Neq  -> jne
        IR.Gt  -> jg
        IR.Ge  -> jge
        IR.Lt  -> jl
        IR.Le  -> jle
  j $ OLabel ltrue
  jmp $ OLabel lfalse


cInstr :: IR.Instr -> Compiler ()
cInstr i = case i of
  IR.Assg t loc e -> do
    vloc <- getLoc loc
    case e of
      IR.Const c ->
        cConst c vloc
      IR.NumOp o l r -> do
        cConst l eax
        cConst r ecx
        cNOp o ecx eax
        mov eax vloc
      IR.RelOp o l r -> do
        cConst l eax
        cConst r ecx
        cROp o ecx eax
        mov eax vloc
      IR.UnOp o v -> do
        cConst v eax
        case o of
          IR.Not -> do
            test eax eax
            setz eax
          IR.Neg ->
            neg eax
      IR.Call f as -> do
        forM_ (reverse as) $ \a -> do
          cConst a eax
          push eax
        call (OLabel f)
        add (OConst $ 4 * fromIntegral (length as)) esp
        mov eax vloc

cFinInstr :: IR.FinInstr -> Compiler ()
cFinInstr i = case i of
  IR.Ret Nothing -> ret
  IR.Ret (Just v) -> do
    cConst v eax
    leave
    ret
  IR.Jmp (IR.Label l) -> jmp (OLabel l)
  IR.Br c (IR.Label ltrue) (IR.Label lfalse) -> case c of
    IR.CondConst cc -> do
      cConst cc eax
      test eax eax
      jne $ OLabel ltrue
      jmp $ OLabel lfalse
    IR.Cond o l r -> do
      cConst l eax
      cConst r ecx
      cmp ecx eax
      cROpJmp o ltrue lfalse
  IR.Unreachable -> return ()

cBlock :: IR.Block -> Compiler ()
cBlock (IR.Block (IR.Label bname) instrs fin) = do
  label bname
  forM_ instrs cInstr
  cFinInstr fin

cRoutine :: IR.Routine -> Compiler ()
cRoutine (IR.Routine (IR.Label rname) args blocks) = do
  globl rname
  let locals = S.unions (map IR.getUsedVars blocks) S.\\ S.fromList args
      routineVarEnv = M.fromList $ localSupply ++ argSupply where
        localSupply = flip evalState 1 $
          mapM (\v -> do
                   i <- get
                   put (i+1)
                   return (v, mem (-(4::Int) * i) EBP :: Operand)) (S.toList locals)
        argSupply = flip evalState 2 $
          mapM (\v -> do
                   i <- get
                   put (i+1)
                   return (v, mem ((4::Int) * i) EBP :: Operand)) args
  local (set varMap routineVarEnv) $ do
    label rname
    push ebp
    mov esp ebp
    sub (OConst $ fromIntegral $ length locals * 4) esp
    forM_ blocks cBlock

compileStrings :: S.Set String -> Compiler ()
compileStrings strs = forM_ strs $ \s -> do
  let vname = makeStrName s
  l <- makeLabel ".LC"
  string s
  label vname
  long l

cProgram :: IR.IR -> Compiler ()
cProgram (IR.IR routines) = do
  forM_ routines cRoutine
  use strings >>= compileStrings
  return ()

compile :: IR.IR -> Assembly
compile ir = optimize $
  Assembly $ snd $ evalRWS (cProgram ir) undefined (St 1 S.empty)
