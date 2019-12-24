{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Latte.Backend.X86.Compile where

import Data.Traversable
import Control.Monad
import Control.Monad.RWS hiding ((<>))
import Control.Monad.Writer

import qualified Latte.Frontend.IR as IR
import qualified Latte.Frontend.AST as AST
import Latte.Backend.X86.X86

type Compiler = RWS () [Instr] ()

getLoc :: IR.VarId -> Compiler Operand
getLoc = _

cInstr :: IR.Instr -> Compiler ()
cInstr = \case
  IR.Assg t loc e -> do
    vloc <- getLoc loc
    case e of
      IR.Const (IR.CVar c) -> do
        cloc <- getLoc c
        mov cloc eax
        mov eax vloc
      IR.Const (IR.CInt i) -> do
        mov (OConst i) vloc
      IR.Op IR.Add (IR.CVar l) (IR.CVar r) -> do
        lloc <- getLoc l
        rloc <- getLoc r
        mov lloc eax
        add rloc eax
        mov eax vloc

cFinInstr :: IR.FinInstr -> Compiler ()
cFinInstr = \case
  IR.Ret Nothing -> ret
  IR.Ret (Just v) -> do
    case v of
      IR.CVar c -> do
        cloc <- getLoc c
        mov cloc eax
      IR.CInt i -> do
        mov (OConst i) eax
    ret
  IR.Jmp (IR.Label l) -> jmp (OLabel l)

cBlock :: IR.Block -> Compiler ()
cBlock (IR.Block (IR.Label bname) instrs fin) = do
  label bname
  forM_ instrs cInstr
  cFinInstr fin

cRoutine :: IR.Routine -> Compiler ()
cRoutine (IR.Routine (IR.Label rname) args blocks) = do
  push ebp
  mov esp ebp
  _
