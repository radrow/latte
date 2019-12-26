{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Latte.Backend.X86.Compile where

import Text.PrettyPrint.HughesPJClass
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Control.Monad
import Control.Monad.RWS hiding ((<>))
import Control.Monad.Writer
import Control.Monad.State
import Prelude hiding ((<>), or, and)

import qualified Latte.Frontend.IR as IR
import qualified Latte.Frontend.AST as AST
import Latte.Backend.X86.X86 as X86

type VarMap = M.Map IR.VarId Operand

data Env = Env
  { _envVarMap :: VarMap
  }
data St = St
  { _stSupply :: Int
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

makeLabel :: Compiler String
makeLabel = do
  i <- nextSup
  let l = "label" ++ show i
  label l
  return l

cConst :: IR.Const -> Operand -> Compiler ()
cConst ic o = case ic of
  IR.CVar c -> do
    cloc <- getLoc c
    mov cloc eax
    mov eax o
  IR.CInt i -> do
    mov (OConst i) o

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
    test al al -- TODO zjebane

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
cInstr = \case
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

cFinInstr :: IR.FinInstr -> Compiler ()
cFinInstr = \case
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

cBlock :: IR.Block -> Compiler ()
cBlock (IR.Block (IR.Label bname) instrs fin) = do
  label bname
  forM_ instrs cInstr
  cFinInstr fin

cRoutine :: IR.Routine -> Compiler ()
cRoutine (IR.Routine (IR.Label rname) args blocks) = do
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

cProgram :: IR.IR -> Compiler ()
cProgram (IR.IR routines) =
  forM_ routines cRoutine

compile :: IR.IR -> Assembly
compile ir =
  Assembly $ snd $ evalRWS (cProgram ir) undefined (St 1)
