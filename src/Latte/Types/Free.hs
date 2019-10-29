{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module Latte.Types.Free where

import Data.List.NonEmpty
import Control.Monad.Identity
import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Writer

import qualified Latte.Types.Syntax as S(Op)
import Latte.Types.Syntax hiding (Op)


data Op where
  Op :: S.Op t -> Op
deriving instance Show Op


data ExprF prod res
  = ELitF Ann Lit (prod -> res)
  | EAppF Ann Id [prod] (prod -> res)
  | EVarF Ann Id (prod -> res)
  | ENegF Ann prod (prod -> res)
  | ENotF Ann prod (prod -> res)
  | EOpF Ann Op prod prod (prod -> res)
  deriving Functor


type ExprM prod = Free (ExprF prod)
type SemExpr prod = ExprM prod prod

semOp :: Ann -> Op -> SemExpr prod -> SemExpr prod -> SemExpr prod
semOp ann op l r = do
  lf <- l
  rf <- r
  liftF $ EOpF ann op lf rf id

exprM :: Expr t -> ExprM prod prod
exprM = \case
  EOr    ann e1 e2 -> semOp ann (Op $ Or ann) (exprM e1) (exprM e2)
  EAnd   ann e1 e2 -> semOp ann (Op $ And ann) (exprM e1) (exprM e2)
  ERelOp ann op e1 e2 -> semOp ann (Op op) (exprM e1) (exprM e2)
  EAddOp ann op e1 e2 -> semOp ann (Op op) (exprM e1) (exprM e2)
  EMulOp ann op e1 e2 -> semOp ann (Op op) (exprM e1) (exprM e2)
  ENot   ann e -> exprM e >>= \ee -> liftF $ ENotF ann ee id
  ENeg   ann e -> exprM e >>= \ee -> liftF $ ENotF ann ee id
  ELit   ann l -> liftF $ ELitF ann l id
  EApp   ann f args -> do
    regs <- mapM exprM args
    liftF $ EAppF ann f regs id
  EVar   ann v -> liftF $ EVarF ann v id
  EPar   _ e -> exprM e
  ECoe e -> exprM e


data StmtF expprod prod res
  = SAssgF Ann Id expprod (prod -> res)
  | SDeclF Ann Type (NonEmpty (Id, Maybe expprod)) (prod -> res)
  | SIncrF Ann Id (prod -> res)
  | SDecrF Ann Id (prod -> res)
  | SRetF Ann expprod (prod -> res)
  | SVRetF Ann (prod -> res)
  | SCondF Ann expprod prod (prod -> res)
  | SCondElseF Ann expprod prod prod (prod -> res)
  | SWhileF Ann expprod prod (prod -> res)
  | SExpF Ann expprod (prod -> res)
  | SBlockF Ann [prod] (prod -> res)
  | SEmptyF Ann (prod -> res)
  | SLiftExprF (ExprM expprod expprod) (expprod -> res)
  deriving Functor

liftExpr :: Expr t -> StmtM expprod prod expprod
liftExpr e = liftF $ SLiftExprF (exprM e) id

type StmtM expprod prod = Free (StmtF expprod prod)

stmtM :: Stmt -> StmtM expprod prod prod
stmtM = \case
  SAssg ann i val -> do
    e <- liftExpr val
    liftF $ SAssgF ann i e id
  SDecl ann t vs -> do
    vss <- forM vs $ \case
      (i, Nothing) -> pure (i, Nothing)
      (i, Just e)  -> liftExpr e >>= \ee -> pure (i, Just ee)
    liftF $ SDeclF ann t vss id
  SIncr ann i -> liftF $ SIncrF ann i id
  SDecr ann i -> liftF $ SDecrF ann i id
  SRet ann e -> liftExpr e >>= \ee -> liftF $ SRetF ann ee id
  SVRet ann -> liftF $ SVRetF ann id
  SCond ann c t -> do
    cc <- liftExpr c
    tt <- stmtM t
    liftF $ SCondF ann cc tt id
  SCondElse ann c t e -> do
    cc <- liftExpr c
    tt <- stmtM t
    ee <- stmtM e
    liftF $ SCondElseF ann cc tt ee id
  SWhile ann c b -> do
    cc <- liftExpr c
    tt <- stmtM b
    liftF $ SWhileF ann cc tt id
  SExp ann e -> liftExpr e >>= \ee -> liftF $ SExpF ann ee id
  SBlock ann sts -> mapM stmtM sts >>= \stse -> liftF $ SBlockF ann stse id
  SEmpty ann -> liftF $ SEmptyF ann id

data TopDefF expprod prod res
  = FunDefF Ann Type Id [Arg] prod (prod -> res)
  | LiftStmtF (StmtM expprod prod prod) (prod -> res)
  deriving Functor

type TopDefM expprod prod = Free (TopDefF expprod prod)


liftStmt :: Stmt -> TopDefM expprod prod prod
liftStmt s = liftF $ LiftStmtF (stmtM s) id


topDefM :: TopDef -> TopDefM expprod prod prod
topDefM = \case
  FunDef ann t i args stmt -> do
    stmte <- liftStmt stmt
    liftF $ FunDefF ann t i args stmte id

data ProgramF expprod prod res
  = ProgramF [prod] (prod -> res)
  | LiftTopDefF (TopDefM expprod prod prod) (prod -> res)
  deriving Functor

type ProgramM expprod prod = Free (ProgramF expprod prod)

liftTopDef :: TopDef -> ProgramM expprod prod prod
liftTopDef t = liftF $ LiftTopDefF (topDefM t) id

programM :: Program -> ProgramM expprod prod prod
programM (Program ts) = do
  tops <- mapM liftTopDef ts
  liftF $ ProgramF tops id
