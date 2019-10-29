{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module Latte.Types.Free where

import Data.List.NonEmpty
import Control.Monad.Identity
import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Writer

import Latte.Types.Syntax as S
import Latte.Types.AST as A

data ExprF prod res
  = ELitF Ann Lit (prod -> res)
  | EAppF Ann Id [prod] (prod -> res)
  | EVarF Ann Id (prod -> res)
  | ENegF Ann prod (prod -> res)
  | ENotF Ann prod (prod -> res)
  | EOpF Ann A.Op prod prod (prod -> res)
  deriving Functor


type ExprM prod = Free (ExprF prod)


exprM :: A.Expr -> ExprM prod prod
exprM = \case
  A.ELit ann lit -> liftF $ ELitF ann lit id
  A.EApp ann name args -> do
    regs <- mapM exprM args
    liftF $ EAppF ann name regs id
  A.EVar ann name -> liftF $ EVarF ann name id
  A.ENeg ann e -> exprM e >>= \ee -> liftF $ ENegF ann ee id
  A.ENot ann e -> exprM e >>= \ee -> liftF $ ENotF ann ee id
  A.EOp ann op e1 e2 -> do
    e1r <- exprM e1
    e2r <- exprM e2
    liftF $ EOpF ann op e1r e2r id

eint :: ExprM String a -> (String -> a)
eint act = foldFree morph act where
  morph :: forall x. ExprF String x -> (String -> x)
  morph (EVarF _ x k) = \s -> k (S.iName x ++ s)

type FunT d m a = d -> m a

data StmtF expprod prod res
  = SAssgF Ann Id expprod (prod -> res)
  | SDeclF Ann Type (NonEmpty (Id, Maybe expprod)) (prod -> res)
  | SIncrF Ann Id (prod -> res)
  | SDecrF Ann Id (prod -> res)
  | SRetF Ann expprod (prod -> res)
  | SVRetF Ann (prod -> res)
  | SCondF Ann expprod prod (prod -> res)
  | SCondElseF Ann expprod prod prod (prod -> res)
  | SWhileF Ann expprod (Stmt expprod) (prod -> res)
  | SExpF Ann expprod (prod -> res)
  | SBlockF Ann [prod] (prod -> res)
  | SEmptyF Ann (prod -> res)
  | SLiftExprF (ExprM expprod expprod) (expprod -> res)
  deriving Functor

type StmtM expprod prod = Free (StmtF expprod prod)

stmtM :: S.Stmt A.Expr -> StmtM expprod prod prod
stmtM = \case
  SAssg ann i val -> do
    e <- liftF $ SLiftExprF (exprM val) id
    liftF $ SAssgF ann i e id

sint :: StmtM String String a -> (String -> a)
sint act = foldFree morph act where
  morph :: StmtF String String x -> (String -> x)
  morph (SAssgF _ x val k) =
    \s -> k (S.iName x ++ " = " ++ val ++ s)
  morph (SLiftExprF ex k) = k . eint ex

