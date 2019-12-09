{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Latte.Types.AST where

import Data.List.NonEmpty(NonEmpty)

-- import Latte.Types.Syntax hiding (Op, Expr(..), TopDef(..), Program(..))
import Latte.Types.Syntax
import Latte.Types.Latte


data AnyOp where
  Op :: Op t -> AnyOp
deriving instance Show AnyOp

type family ExprDecoration (s :: Stage) where
  ExprDecoration 'Untyped = ()
  ExprDecoration 'Typed = Type 'Typed
  ExprDecoration 'Resolved = Type 'Resolved
type family ProjectionIndex (s :: Stage) where
  ProjectionIndex 'Untyped = Id
  ProjectionIndex 'Typed = Int
class (Show (ExprDecoration s), Show (ProjectionIndex s)) => FullShow s where

data Expr (s :: Stage) where
  ELit  :: Ann -> ExprDecoration s -> Lit -> Expr s
  EApp  :: Ann -> ExprDecoration s -> Id -> [Expr s] -> Expr s
  EVar  :: Ann -> ExprDecoration s -> Id -> Expr s
  ENeg  :: Ann -> ExprDecoration s -> Expr s -> Expr s
  ENot  :: Ann -> ExprDecoration s -> Expr s -> Expr s
  EOp   :: Ann -> ExprDecoration s -> AnyOp -> Expr s -> Expr s -> Expr s
  EProj :: Ann -> ExprDecoration s -> Expr s -> ProjectionIndex s -> Expr s
  EMApp :: Unresolved s =>
           Ann -> ExprDecoration s -> Expr s  -> ProjectionIndex s -> Expr s
deriving instance FullShow s => Show (Expr s)

data Stmt (e :: Stage)
  = SAssg Ann Id (Expr e) (Stmt e)
  | SDecl Ann (Type e) (NonEmpty (Id, Maybe (Expr e))) (Stmt e)
  | SIncr Ann Id (Stmt e)
  | SDecr Ann Id (Stmt e)
  | SRet Ann (Expr e)
  | SVRet Ann
  | SCond Ann (Expr e) (Stmt e) (Stmt e)
  | SCondElse Ann (Expr e) (Stmt e) (Stmt e) (Stmt e)
  | SWhile Ann (Expr e) (Stmt e) (Stmt e)
  | SExp Ann (Expr e) (Stmt e)
  | SBlock Ann (Stmt e) (Stmt e)
  | SEmpty
deriving instance FullShow s => Show (Stmt s)


entailExpr :: RawExpr t -> Expr Untyped
entailExpr = \case
  REOr    ann e1 e2 -> EOp ann () (Op $ Or ann)  (entailExpr e1) (entailExpr e2)
  REAnd   ann e1 e2 -> EOp ann () (Op $ And ann) (entailExpr e1) (entailExpr e2)
  RERelOp ann op e1 e2 -> EOp ann () (Op op) (entailExpr e1) (entailExpr e2)
  REAddOp ann op e1 e2 -> EOp ann () (Op op) (entailExpr e1) (entailExpr e2)
  REMulOp ann op e1 e2 -> EOp ann () (Op op) (entailExpr e1) (entailExpr e2)
  RENot   ann e -> ENot ann () (entailExpr e)
  RENeg   ann e -> ENeg ann () (entailExpr e)
  RELit   ann l -> ELit ann () l
  REApp   ann f args -> EApp ann () f (map entailExpr args)
  REVar   ann v -> EVar ann () v
  REPar   _ e -> entailExpr e
  RECoe   e -> entailExpr e

entailStmt :: RawStmt -> Stmt Untyped
entailStmt s =
  let entailSingle :: RawStmt -> Stmt Untyped -> Stmt Untyped
      entailSingle = \case
        RSAssg ann i v -> SAssg ann i (entailExpr v)
        RSDecl ann t vs -> SDecl ann t $ fmap (\(i, em) -> (i, entailExpr <$> em)) vs
        RSIncr ann i -> SIncr ann i
        RSDecr ann i -> SDecr ann i
        RSRet ann e -> const $ SRet ann (entailExpr e)
        RSVRet ann -> const $ SVRet ann
        RSCond ann c t -> SCond ann (entailExpr c) (entailStmt t)
        RSCondElse ann c t e -> SCondElse ann (entailExpr c) (entailStmt t) (entailStmt e)
        RSWhile ann c b -> SWhile ann (entailExpr c) (entailStmt b)
        RSExp ann e -> SExp ann (entailExpr e)
        RSBlock ann sts -> SBlock ann (composeSts $ map entailSingle sts)
        RSEmpty ann -> const $ SEmpty
      composeSts fs = foldr (.) id fs $ SEmpty
  in entailSingle s SEmpty


getExprDec :: Expr s -> ExprDecoration s
getExprDec = \case
  ELit _ a _ -> a
  EVar _ a _ -> a
  EApp _ a _ _ -> a
  ENeg _ a _ -> a
  ENot _ a _ -> a
  EOp  _ a _ _ _ -> a


instance HasAnn (Expr p) where
  getAnn = \case
    ELit a _ _ -> a
    EVar a _ _ -> a
    EApp a _ _ _ -> a
    ENeg a _ _ -> a
    ENot a _ _ -> a
    EOp  a _ _ _ _ -> a


data TopDef (s :: Stage)
  = TopFun
    { tdAnn :: Ann
    , tdType :: Type s
    , tdId :: Id
    , tdArgs :: [Arg s]
    , tdFunBody :: Stmt s
    }
  | TopClass
    { tdAnn :: Ann
    , tdId :: Id
    , tdSuper :: Maybe Id
    , tdClassBody :: [ClassMember s]
    }
deriving instance FullShow s => Show (TopDef s)


data ClassMember (s :: Stage)
  = Method
    { cmAnn :: Ann
    , cmAccess :: ClassMemberAccess
    , cmPlace :: ClassMemberPlace
    , cmRetType :: Type s
    , cmId :: Id
    , cmArgs :: [Arg s]
    , cmBody :: Stmt s
    }
  | AbstractMethod
    { cmAnn :: Ann
    , cmAccess :: ClassMemberAccess
    , cmPlace :: ClassMemberPlace
    , cmRetType :: Type s
    , cmId :: Id
    , cmArgs :: [Arg s]
    }
  | Field
    { cmAnn :: Ann
    , cmAccess :: ClassMemberAccess
    , cmPlace :: ClassMemberPlace
    , cmType :: Type s
    , cmAssignments :: (NonEmpty (Id, Maybe (Expr s)))
    }
  | Constructor
    { cmAnn :: Ann
    , cmAccess :: ClassMemberAccess
    , cmOptId :: (Maybe Id)
    , cmArgs :: [Arg s]
    , cmBody :: Stmt s
    }
deriving instance FullShow s => Show (ClassMember s)


data ClassMemberPlace = Dynamic | Static
  deriving (Show)
data ClassMemberAccess = Private | Public
  deriving (Show)

newtype Program (s :: Stage) = Program [TopDef s]
deriving instance FullShow s => Show (Program s)


instance HasAnn (Stmt s) where
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
