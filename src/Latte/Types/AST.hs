{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Latte.Types.AST where

import Data.List.NonEmpty(NonEmpty)

-- import Latte.Types.Syntax hiding (Op, Expr(..), TopDef(..), Program(..))
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

data Stmt e
  = SAssg Ann Id e (Stmt e)
  | SDecl Ann Type (NonEmpty (Id, Maybe e)) (Stmt e)
  | SIncr Ann Id (Stmt e)
  | SDecr Ann Id (Stmt e)
  | SRet Ann e
  | SVRet Ann
  | SCond Ann e (Stmt e) (Stmt e)
  | SCondElse Ann e (Stmt e) (Stmt e) (Stmt e)
  | SWhile Ann e (Stmt e) (Stmt e)
  | SExp Ann e (Stmt e)
  | SBlock Ann (Stmt e) (Stmt e)
  | SEmpty
  deriving (Show)

data TopDef p
  = TopFun Ann Type Id [Arg] (Stmt (Expr p))
  | TopClass Ann Id [ClassMember]
  deriving (Show)

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

entailStmt :: S.Stmt -> Stmt (Expr ())
entailStmt s =
  let entailSingle :: S.Stmt -> (Stmt (Expr ()) -> Stmt (Expr ()))
      entailSingle = \case
        S.SAssg ann i v -> SAssg ann i (entailExpr v)
        S.SDecl ann t vs -> SDecl ann t $ fmap (\(i, em) -> (i, entailExpr <$> em)) vs
        S.SIncr ann i -> SIncr ann i
        S.SDecr ann i -> SDecr ann i
        S.SRet ann e -> const $ SRet ann (entailExpr e)
        S.SVRet ann -> const $ SVRet ann
        S.SCond ann c t -> SCond ann (entailExpr c) (entailStmt t)
        S.SCondElse ann c t e -> SCondElse ann (entailExpr c) (entailStmt t) (entailStmt e)
        S.SWhile ann c b -> SWhile ann (entailExpr c) (entailStmt b)
        S.SExp ann e -> SExp ann (entailExpr e)
        S.SBlock ann sts -> SBlock ann (composeSts $ map entailSingle sts)
        S.SEmpty ann -> const $ SEmpty
      composeSts fs = foldr (.) id fs $ SEmpty
  in entailSingle s SEmpty

entailTopDef :: S.TopDef -> TopDef ()
entailTopDef = \case
  S.TopFun ann t name args body ->
    TopFun ann t name args (entailStmt body)

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


instance HasAnn (Stmt e) where
  getAnn = \case
    SAssg a _ _ _ -> a
    SDecl a _ _ _ -> a
    SIncr a _ _ -> a
    SDecr a _ _ -> a
    SRet a _ -> a
    SVRet a -> a
    SCond a _ _ _ -> a
    SCondElse a _ _ _ _ -> a
    SWhile a _ _ _ -> a
    SExp a _ _ -> a
    SBlock a _ _ -> a
    SEmpty -> fakeAnn


getExprDec :: Expr a -> a
getExprDec = \case
  ELit _ a _ -> a
  EVar _ a _ -> a
  EApp _ a _ _ -> a
  ENeg _ a _ -> a
  ENot _ a _ -> a
  EOp  _ a _ _ _ -> a
