{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Latte.Types.AST where

import Data.List.NonEmpty(NonEmpty)

import qualified Latte.Types.Syntax as S
import           Latte.Types.Syntax
  ( Lit, Ann, Id, Type, Arg, Stmt(..)
  )

data Op where
  Op :: S.Op t -> Op
deriving instance Show Op

data Expr
  = ELit Ann Lit
  | EApp Ann Id [Expr]
  | EVar Ann Id
  | ENeg Ann Expr
  | ENot Ann Expr
  | EOp  Ann Op Expr Expr
deriving instance Show Expr


data FunDef = FunDef Ann Type Id [Arg] (Stmt Expr)

newtype Program = Program [FunDef]


entailExpr :: S.Expr t -> Expr
entailExpr = \case
  S.EOr    ann e1 e2 -> EOp ann (Op $ S.Or ann)  (entailExpr e1) (entailExpr e2)
  S.EAnd   ann e1 e2 -> EOp ann (Op $ S.And ann) (entailExpr e1) (entailExpr e2)
  S.ERelOp ann op e1 e2 -> EOp ann (Op op) (entailExpr e1) (entailExpr e2)
  S.EAddOp ann op e1 e2 -> EOp ann (Op op) (entailExpr e1) (entailExpr e2)
  S.EMulOp ann op e1 e2 -> EOp ann (Op op) (entailExpr e1) (entailExpr e2)
  S.ENot   ann e -> ENot ann (entailExpr e)
  S.ENeg   ann e -> ENeg ann (entailExpr e)
  S.ELit   ann l -> ELit ann l
  S.EApp   ann f args -> EApp ann f (map entailExpr args)
  S.EVar   ann v -> EVar ann v
  S.EPar   _ e -> entailExpr e
  S.ECoe e -> entailExpr e

entailStmt :: Stmt (S.Expr 0) -> Stmt Expr
entailStmt = \case
  SAssg ann i v -> SAssg ann i (entailExpr v)
  SDecl ann t vs -> SDecl ann t $ fmap (\(i, em) -> (i, entailExpr <$> em)) vs
  SIncr ann i -> SIncr ann i
  SDecr ann i -> SDecr ann i
  SRet ann e -> SRet ann (entailExpr e)
  SVRet ann -> SVRet ann
  SCond ann c t -> SCond ann (entailExpr c) (entailStmt t)
  SCondElse ann c t e -> SCondElse ann (entailExpr c) (entailStmt t) (entailStmt e)
  SWhile ann c b -> SWhile ann (entailExpr c) (entailStmt b)
  SExp ann e -> SExp ann (entailExpr e)
  SBlock ann sts -> SBlock ann (map entailStmt sts)
  SEmpty ann -> SEmpty ann

entailFunDef :: S.FunDef -> FunDef
entailFunDef (S.FunDef ann t name args body) =
  FunDef ann t name args (entailStmt body)

entailProgram :: S.Program -> Program
entailProgram (S.Program funs) = Program (map entailFunDef funs)
