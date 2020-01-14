{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module Latte.Frontend.ConstantOptimizer where

import Latte.Frontend.AST
import Control.Monad.State
import Control.Monad.Except
import Control.Monad
import Control.Lens
import Control.Lens.Extras
import qualified Data.Map as M
import Prelude hiding (LT, GT, EQ)

type ValMap = M.Map VarId Lit

data St = St
  { _stValMap :: ValMap
  , _stEarlyStop :: Bool
  }
makeLensesWith abbreviatedFields ''St

type Optimizer = State St

pattern LI :: Integer -> Expr 'Typed
pattern LI i <- ELit _ _ (LInt i)
pattern LB :: Bool -> Expr 'Typed
pattern LB i <- ELit _ _ (LBool i)

getVar :: VarId -> Optimizer (Maybe Lit)
getVar v = uses valMap (M.lookup v)

assignVar :: VarId -> Expr 'Typed -> Optimizer ()
assignVar v e = do
  oExpr e >>= \case
    ELit _ _ x -> modify $ over valMap (M.insert v x)
    _ -> pure ()

litI :: Ann -> Type -> Integer -> Optimizer (Expr 'Typed)
litI a t i = return $ ELit a t $ LInt i

litB :: Ann -> Type -> Bool -> Optimizer (Expr 'Typed)
litB a t i = return $ ELit a t $ LBool i

stop :: a -> Optimizer a
stop a = modify (set earlyStop True) >> pure a

oExpr :: Expr 'Typed -> Optimizer (Expr 'Typed)
oExpr ex = case ex of
  ELit _ _ _ -> return ex
  EVar a t v -> maybe ex (ELit a t) <$> getVar v
  EApp _ _ fname as | fname == "error" -> stop ex
  EApp _ _ fname as -> return ex
  EUnOp a t o e -> do
    v <- oExpr e
    case (o, v) of
      (Neg, LI i) -> litI a t (-i)
      (Not, LB b) -> litB a t (not b)
      _ -> return ex
  EOp a t (Op o) l r -> do
    ll <- oExpr l
    rr <- oExpr r
    case (ll, rr, o) of
      (LI il, LI ir, _) -> case o of
        LT _    -> litB a t (il < ir)
        LEQ _   -> litB a t (il <= ir)
        EQ _    -> litB a t (il == ir)
        NEQ _   -> litB a t (il /= ir)
        GEQ _   -> litB a t (il >= ir)
        GT _    -> litB a t (il > ir)
        Plus _  -> litI a t (il + ir)
        Minus _ -> litI a t (il - ir)
        Mult _  -> litI a t (il * ir)
        Div _   -> case ir of
          0 -> stop (EOp a t (Op o) ll rr)
          _ -> litI a t (il `div` ir)
        Mod _   -> litI a t (il `mod` ir)
        _ -> return ex

      (LI 0, rv, Plus _) -> return rv
      (lv, LI 0, Plus _) -> return lv

      (LI 0, _, Minus _) -> return (EUnOp a t Neg r)
      (lv, LI 0, Minus _) -> return lv

      (LI 0, _, Mult _) -> litI a t 0
      (_, LI 0, Mult _) -> litI a t 0

      (LI 0, _, Div _) -> litI a t 0
      (_, LI 0, Div _) -> stop ex

      (LI 1, rv, Mult _) -> return rv
      (lv, LI 1, Mult _) -> return lv

      (LI 1, rv, Div _) -> return rv
      (lv, LI 1, Div _) -> return lv

      (LB True, rv, And _) -> return rv
      (lv, LB True, And _) -> return lv

      (LB False, _, And _) -> litB a t False
      (_, LB False, And _) -> litB a t False

      (LB False, rv, Or _) -> return rv
      (lv, LB False, Or _) -> return lv

      (LB True, _, Or _) -> litB a t True
      (_, LB True, Or _) -> litB a t True
      _ -> return ex
  _ -> return ex


oStmtScoped :: Stmt 'Typed -> Optimizer (Stmt 'Typed)
oStmtScoped ob = do
  backup <- use earlyStop
  modify $ set earlyStop False
  res <- oStmt ob
  modify $ set earlyStop backup
  return res

continue :: (Stmt 'Typed -> Stmt 'Typed) -> Stmt 'Typed -> Optimizer (Stmt 'Typed)
continue b k = do
  break <- use earlyStop
  if break then return (b (SEmpty fakeAnn)) else b <$> oStmt k

oStmt :: Stmt 'Typed -> Optimizer (Stmt 'Typed)
oStmt s = case s of
  SDecl a t v k -> do
    continue (SDecl a t v) k
  SAssg a v e k -> do
    oe <- oExpr e
    assignVar v oe
    continue (SAssg a v oe) k
  SFieldAssg a b f e k -> do
    oe <- oExpr e
    continue (SFieldAssg a b f oe) k
  SIncr a v k -> do
    getVar v >>= \case
      Just (LInt i) -> modify $ over valMap $ M.insert v (LInt $ i+1)
      _ -> pure ()
    continue (SIncr a v) k
  SDecr a v k -> do
    getVar v >>= \case
      Just (LInt i) -> modify $ over valMap $ M.insert v (LInt $ i-1)
      _ -> pure ()
    continue (SIncr a v) k
  SCond a c t k -> do
    oc <- oExpr c
    case oc of
      LB True ->
        oStmt $ SBlock a t k
      LB False ->
        oStmt k
      _ -> do
        ot <- oStmtScoped t
        continue (SCond a oc ot) k
  SCondElse a c t e k -> do
    oc <- oExpr c
    case oc of
      LB True ->
        oStmt $ SBlock a t k
      LB False ->
        oStmt $ SBlock a e k
      _ -> do
        ot <- oStmtScoped t
        oe <- oStmtScoped e
        continue (SCondElse a oc ot oe) k
  SWhile a c b k -> do
    oc <- oExpr c
    case oc of
      LB True ->
        stop (SWhile a c b (SEmpty fakeAnn))
      LB False ->
        oStmt k
      _ -> do
        ob <- oStmtScoped b
        continue (SWhile a oc ob) k
  SExp a e k -> do
    oe <- oExpr e
    case oe of
      EApp _ _ _ _ -> do
        continue (SExp a oe) k
      EMApp _ _ _ _ _ -> do
        continue (SExp a oe) k
      _ -> oStmt k
  SRet a e _ -> do
    oe <- oExpr e
    stop $ (SRet a oe (SEmpty fakeAnn))
  SVRet a _ -> stop $ SVRet a (SEmpty fakeAnn)
  SBlock a (SBlock _ b (SEmpty _)) k -> oStmt (SBlock a b k)
  SBlock _ (SEmpty _) k -> oStmt k
  SBlock a b k -> do
    ob <- oStmt b
    continue (SBlock a ob) k
  SEmpty a -> pure $ SEmpty a

optimizeBody :: Stmt 'Typed -> Stmt 'Typed
optimizeBody s = evalState (oStmt s) (St M.empty False)
