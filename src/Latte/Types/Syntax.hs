{-# LANGUAGE StandaloneDeriving #-}
module Latte.Types.Syntax
  ( Id(..), Ann(..), Lit(..), Op(..), OpType(..), Expr(..), Stmt(..)
  , Type(..), Arg(..), FunDef(..), Program(..)
  ) where

import Data.List.NonEmpty(NonEmpty)
import GHC.TypeNats(Nat, type (+))


newtype Id = Id {iName :: String}
  deriving (Show, Eq, Ord)


data Ann = Ann { file :: FilePath, line :: Int, column :: Int }
  deriving (Show)


data Lit
  = LInt Int
  | LString String
  | LBool Bool
  deriving (Show)


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


data Stmt expr
  = SAssg Ann Id expr
  | SDecl Ann Type (NonEmpty (Id, Maybe expr))
  | SIncr Ann Id
  | SDecr Ann Id
  | SRet Ann expr
  | SVRet Ann
  | SCond Ann expr (Stmt expr)
  | SCondElse Ann expr (Stmt expr) (Stmt expr)
  | SWhile Ann expr (Stmt expr)
  | SExp Ann expr
  | SBlock Ann [Stmt expr]
  | SEmpty Ann
  deriving (Show)


data Type = TInt Ann | TString Ann | TBool Ann | TFun Ann [Type] Type | TVoid Ann
  deriving (Show)


data Arg = Arg Ann Type Id
  deriving (Show)


data FunDef = FunDef Ann Type Id [Arg] (Stmt (Expr 0))
  deriving (Show)


newtype Program = Program [FunDef]
  deriving (Show)
