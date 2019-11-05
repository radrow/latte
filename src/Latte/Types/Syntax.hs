{-# LANGUAGE StandaloneDeriving #-}
module Latte.Types.Syntax
  ( Op(..), OpType(..), Expr(..), Stmt(..)
  , Arg(..), TopDef(..), Program(..)
  ) where

import Data.List.NonEmpty(NonEmpty)
import GHC.TypeNats(Nat, type (+))
import Latte.Types.Latte(Id, Ann, Lit, Type)


data OpType = Rel | Add | Mul | Log
  deriving (Show)


data Op (t :: OpType) where
  LT    :: Ann -> Op 'Rel
  LEQ   :: Ann -> Op 'Rel
  EQ    :: Ann -> Op 'Rel
  NEQ   :: Ann -> Op 'Rel
  GEQ   :: Ann -> Op 'Rel
  GT    :: Ann -> Op 'Rel
  Plus  :: Ann -> Op 'Add
  Minus :: Ann -> Op 'Add
  Mult  :: Ann -> Op 'Mul
  Div   :: Ann -> Op 'Mul
  Mod   :: Ann -> Op 'Mul
  Or    :: Ann -> Op 'Log
  And   :: Ann -> Op 'Log
deriving instance Show (Op t)


data Expr (l :: Nat) where
  EOr    :: Ann -> Expr 1 -> Expr 0            -> Expr 0
  EAnd   :: Ann -> Expr 2 -> Expr 1            -> Expr 1
  ERelOp :: Ann -> Op 'Rel -> Expr 2 -> Expr 3 -> Expr 2
  EAddOp :: Ann -> Op 'Add -> Expr 3 -> Expr 4 -> Expr 3
  EMulOp :: Ann -> Op 'Mul -> Expr 4 -> Expr 5 -> Expr 4
  ENot   :: Ann -> Expr 6                      -> Expr 5
  ENeg   :: Ann -> Expr 6                      -> Expr 5
  ELit   :: Ann -> Lit                         -> Expr 6
  EApp   :: Ann -> Id -> [Expr 0]              -> Expr 6
  EVar   :: Ann -> Id                          -> Expr 6
  EPar   :: Ann -> Expr 0                      -> Expr 6
  ECoe   ::        Expr (n + 1)                -> Expr n
deriving instance Show (Expr n)


type E = Expr 0


data Stmt
  = SAssg Ann Id E
  | SDecl Ann Type (NonEmpty (Id, Maybe E))
  | SIncr Ann Id
  | SDecr Ann Id
  | SRet Ann E
  | SVRet Ann
  | SCond Ann E Stmt
  | SCondElse Ann E Stmt Stmt
  | SWhile Ann E Stmt
  | SExp Ann E
  | SBlock Ann [Stmt]
  | SEmpty Ann
  deriving (Show)


data Arg = Arg Ann Type Id
  deriving (Show)


data TopDef = FunDef Ann Type Id [Arg] Stmt
  deriving (Show)


newtype Program = Program [TopDef]
  deriving (Show)
