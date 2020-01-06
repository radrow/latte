{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
module Latte.Backend.X86.Compile where

import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.RWS.Lazy hiding ((<>))
import Control.Monad.State
import Prelude hiding ((<>), or, and)

import qualified Latte.Frontend.IR as IR
import qualified Latte.Frontend.AST as AST
import Latte.Backend.X86.X86 as X86
import Latte.Backend.X86.PeepHole

type VarMap = M.Map IR.VarId Operand

data Env = Env
  { _envVarMap :: VarMap
  }
data St = St
  { _stSupply :: Int
  , _stStrings :: M.Map String String
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

makeStrName :: String -> Compiler String
makeStrName s = do
  i <- nextSup
  return $ "local_str_" ++ filter isAlphaNum s ++ "_" ++ show i

requestStr :: String -> Compiler String
requestStr s = do
  v <- makeStrName s
  modify $ over strings (M.insert s v)
  return v

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
    sn <- requestStr s
    if isReg o
      then mov (OVar sn) o
      else do
      mov (OVar sn) edx
      mov edx o

cNOp :: IR.NumOp -> Operand -> Operand -> Compiler ()
cNOp = \case
  IR.Add -> add
  IR.Sub -> sub
  IR.Mul -> \a b -> do
    mov b eax
    mov a ecx
    cdq
    imul ecx
    mov eax b
  IR.Div -> \a b -> do
    mov b eax
    mov a ecx
    cdq
    idiv ecx
    mov eax b
  IR.Mod -> \a b -> do
    mov b eax
    mov a ecx
    cdq
    idiv ecx
    mov edx b
  IR.And -> and
  IR.Or  -> or

cROp :: IR.RelOp -> Operand -> Operand -> Compiler ()
cROp o l r = do
  let i = case o of
        IR.Eq  -> sete
        IR.Neq  -> setne
        IR.Gt  -> setg
        IR.Ge  -> setge
        IR.Lt  -> setl
        IR.Le  -> setle
  cmp l r
  i al
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

sizeOf :: IR.Type -> Compiler Int
sizeOf = \case
  IR.TObj _ flds -> return $ (length flds + 1) * 4
  _ -> return 4

makeMethodDispatcherName :: String -> String
makeMethodDispatcherName s = "__" ++ s ++ "__dispatch"

cExpr :: IR.Type -> Operand -> IR.Expr -> Compiler ()
cExpr t vloc e = case e of
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
    mov eax vloc
  IR.Call f as -> do
    forM_ (reverse as) $ \a -> do
      cConst a eax
      push eax
    call (OLabel f)
    add (OConst $ 4 * fromIntegral (length as)) esp
    mov eax vloc
  IR.Proj v off -> do
    cConst v eax
    mov (OConst $ fromIntegral off) edx
    mov (mem (0 :: Int) (EAX, EDX, 4 :: Int)) eax
    mov eax vloc
  IR.NewObj -> do
    s <- sizeOf t
    let IR.TObj idx _ = t
    push (OConst $ toInteger s)
    call (OLabel "malloc")
    add (OConst 4) esp
    mov (OConst $ fromIntegral idx) (mem EAX)
    mov eax vloc
  IR.VCall f as -> do
    forM_ (reverse as) $ \a -> do
      cConst a eax
      push eax
    call (OLabel $ makeMethodDispatcherName f)
    add (OConst $ 4 * fromIntegral (length as + 1)) esp
    mov eax vloc

cInstr :: IR.Instr -> Compiler ()
cInstr i = comment (AST.pp i) >> case i of
  IR.Assg t loc e -> do
    vloc <- getLoc loc
    cExpr t vloc e
  IR.FieldAssg t b f e -> do
    cConst b ecx
    push ecx
    mov (OConst $ fromIntegral f) edx
    cExpr t eax e
    pop ecx
    mov eax (mem (0 :: Int) (ECX, EDX, 4 :: Int))

cFinInstr :: IR.FinInstr -> Compiler ()
cFinInstr i = comment (AST.pp i) >> case i of
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
  IR.Unreachable ->
    call (OLabel "error")

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

cMethodDispatcher :: AST.Id -> M.Map String [Int] -> Compiler ()
cMethodDispatcher m implMap = do
  label (makeMethodDispatcherName $ AST.iName m)
  push ebp
  mov esp ebp
  mov (mem (8 :: Int) EBP :: Operand) eax
  mov (mem EAX) eax
  forM_ (M.toList implMap) $ \(impl, cids) ->
    void $ mfix $ \l -> do
      void $ mfix $ \lf -> do
        forM_ cids $ \cid -> do
          mov (OConst $ fromIntegral cid) ecx
          cmp eax ecx
          je $ OLabel lf
        jmp $ OLabel l
        makeLabel "dispatch_found_"
      call (OLabel impl)
      leave
      ret
      makeLabel "dispatch_step_"
  call (OLabel "error")

compileStrings :: M.Map String String -> Compiler ()
compileStrings strs = forM_ (M.toList strs) $ \(s, v) -> do
  l <- makeLabel ".LC"
  string s
  label v
  long l

cProgram :: IR.IR -> Compiler ()
cProgram (IR.IR routines) = do
  forM_ routines cRoutine
  use strings >>= compileStrings
  return ()

compile :: IR.MethodMap -> IR.IR -> Assembly
compile mm ir = optimize $
  let act = do
        cProgram ir
        mapM (uncurry cMethodDispatcher) mm
  in Assembly $ snd $ evalRWS act undefined (St 1 M.empty)
