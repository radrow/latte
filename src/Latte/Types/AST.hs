{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Latte.Types.AST where

import Data.List.NonEmpty(NonEmpty)

import Latte.Types.Syntax hiding (Op, Expr(..), TopDef(..), Program(..))
import qualified Latte.Types.Syntax as S
import Latte.Types.Latte


data Op where
  Op :: S.Op t -> Op
deriving instance Show Op

data Expr p
  = ELit Ann p Lit
  | EApp Ann p Id [Expr p]
  | EVar Ann p Id
  | ENeg Ann p (Expr p)
  | ENot Ann p (Expr p)
  | EOp  Ann p Op (Expr p) (Expr p)
deriving instance Show p => Show (Expr p)


data TopDef p = FunDef Ann Type Id [Arg] (Stmt (Expr p))

newtype Program p = Program [TopDef p]


entailExpr :: S.Expr t -> Expr ()
entailExpr = \case
  S.EOr    ann e1 e2 -> EOp ann () (Op $ S.Or ann)  (entailExpr e1) (entailExpr e2)
  S.EAnd   ann e1 e2 -> EOp ann () (Op $ S.And ann) (entailExpr e1) (entailExpr e2)
  S.ERelOp ann op e1 e2 -> EOp ann () (Op op) (entailExpr e1) (entailExpr e2)
  S.EAddOp ann op e1 e2 -> EOp ann () (Op op) (entailExpr e1) (entailExpr e2)
  S.EMulOp ann op e1 e2 -> EOp ann () (Op op) (entailExpr e1) (entailExpr e2)
  S.ENot   ann e -> ENot ann () (entailExpr e)
  S.ENeg   ann e -> ENeg ann () (entailExpr e)
  S.ELit   ann l -> ELit ann () l
  S.EApp   ann f args -> EApp ann () f (map entailExpr args)
  S.EVar   ann v -> EVar ann () v
  S.EPar   _ e -> entailExpr e
  S.ECoe   e -> entailExpr e

entailStmt :: Stmt (S.Expr 0) -> Stmt (Expr ())
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

entailFunDef :: S.TopDef -> TopDef ()
entailFunDef (S.FunDef ann t name args body) =
  FunDef ann t name args (entailStmt body)

entailProgram :: S.Program -> Program ()
entailProgram (S.Program funs) = Program (map entailFunDef funs)


instance HasAnn (Expr p) where
  getAnn = \case
    ELit a _ _ -> a
    EVar a _ _ -> a
    EApp a _ _ _ -> a
    ENeg a _ _ -> a
    ENot a _ _ -> a
    EOp  a _ _ _ _ -> a

instance HasAnn (TopDef p) where
  getAnn = \case
    FunDef a _ _ _ _ -> a


getExprDec :: Expr a -> a
getExprDec = \case
  ELit _ a _ -> a
  EVar _ a _ -> a
  EApp _ a _ _ -> a
  ENeg _ a _ -> a
  ENot _ a _ -> a
  EOp  _ a _ _ _ -> a
