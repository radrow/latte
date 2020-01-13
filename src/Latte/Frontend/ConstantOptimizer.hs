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

data Stop = WillCrash (Expr 'Typed) | EarlyReturn (Stmt 'Typed)

data St = St
  { _stValMap :: ValMap
  , _stStmtBuild :: Stmt 'Typed -> Stmt 'Typed
  }
makeLensesWith abbreviatedFields ''St

type Optimizer = ExceptT Stop (State St)

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

deferStop :: (a -> Optimizer a) -> a -> Optimizer (Maybe Stop, a)
deferStop opt ob =
  catchError (opt ob >>= \o -> return (Nothing, o)) $ \er -> return (Just er, ob)

destroyStop :: Maybe Stop -> Optimizer ()
destroyStop = \case
  Nothing -> return ()
  Just e -> throwError e

oExpr :: Expr 'Typed -> Optimizer (Expr 'Typed)
oExpr ex = case ex of
  ELit _ _ _ -> return ex
  EVar a t v -> maybe ex (ELit a t) <$> getVar v
  EApp _ _ fname as | fname == "error" -> throwError $ WillCrash ex
  EApp _ _ fname as -> return ex
  EUnOp a t o e -> do
    v <- oExpr e
    case (o, v) of
      (Neg, LI i) -> litI a t (-i)
      (Not, LB b) -> litB a t (not b)
      _ -> return ex
  EOp a t (Op o) l r -> do
    (cl, ll) <- deferStop oExpr l
    (cr, rr) <- deferStop oExpr r
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
          0 -> throwError (WillCrash ex)
          _ -> litI a t (il `div` ir)
        Mod _   -> litI a t (il `mod` ir)
        _ -> return ex

      (LI 0, rv, Plus _) -> destroyStop cr >> return rv
      (lv, LI 0, Plus _) -> destroyStop cl >> return lv

      (LI 0, _, Minus _) -> destroyStop cr >> return (EUnOp a t Neg r)
      (lv, LI 0, Minus _) -> destroyStop cl >> return lv

      (LI 0, _, Mult _) -> litI a t 0
      (_, LI 0, Mult _) -> litI a t 0

      (LI 0, _, Div _) -> litI a t 0
      (_, LI 0, Div _) -> throwError (WillCrash ex)

      (LI 1, rv, Mult _) -> destroyStop cr >> return rv
      (lv, LI 1, Mult _) -> destroyStop cl >> return lv

      (LI 1, rv, Div _) -> destroyStop cr >> return rv
      (lv, LI 1, Div _) -> destroyStop cl >> return lv

      (LB True, rv, And _) -> destroyStop cr >> return rv
      (lv, LB True, And _) -> destroyStop cl >> return lv

      (LB False, _, And _) -> litB a t False
      (_, LB False, And _) -> litB a t False

      (LB False, rv, Or _) -> destroyStop cr >> return rv
      (lv, LB False, Or _) -> destroyStop cl >> return lv

      (LB True, _, Or _) -> litB a t True
      (_, LB True, Or _) -> litB a t True
      _ -> return ex
  _ -> return ex


oStmtScoped :: Bool -> Stmt 'Typed -> Optimizer (Stmt 'Typed)
oStmtScoped surelyEval ob = do
  backup <- use stmtBuild
  modify $ set stmtBuild id
  res <- catchError (oStmt ob) $ \e -> if surelyEval then throwError e else
    case e of
      WillCrash _ -> return ob
      EarlyReturn r -> return r
  modify $ set stmtBuild backup
  return res

continue :: (Stmt 'Typed -> Stmt 'Typed) -> Stmt 'Typed -> Optimizer (Stmt 'Typed)
continue b k = do
  modify (over stmtBuild (.b))
  b <$> oStmt k

earlyRet :: (Stmt 'Typed -> Stmt 'Typed) -> Optimizer (Stmt 'Typed)
earlyRet b = do
  build <- use stmtBuild
  throwError (EarlyReturn $ build . b $ (SEmpty fakeAnn))

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
        ot <- oStmtScoped False t
        continue (SCond a oc ot) k
  SCondElse a c t e k -> do
    oc <- oExpr c
    case oc of
      LB True ->
        oStmt $ SBlock a t k
      LB False ->
        oStmt $ SBlock a e k
      _ -> do
        ot <- oStmtScoped False t
        oe <- oStmtScoped False e
        continue (SCondElse a oc ot oe) k
  SWhile a c b k -> do
    oc <- oExpr c
    case oc of
      LB True ->
        earlyRet (SWhile a c b)
      LB False ->
        oStmt k
      _ -> do
        ob <- oStmtScoped False b
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
    earlyRet $ SRet a oe
  SVRet a _ -> earlyRet $ SVRet a
  SBlock a (SBlock _ b (SEmpty _)) k -> oStmt (SBlock a b k)
  SBlock _ (SEmpty _) k -> oStmt k
  SBlock a b k -> do
    ob <- catchError (oStmtScoped True b) $ \e -> case e of
      EarlyReturn r -> earlyRet (SBlock a r)
      _ -> throwError e
    continue (SBlock a ob) k
  SEmpty a -> pure $ SEmpty a

optimizeBody :: Stmt 'Typed -> Stmt 'Typed
optimizeBody s = case evalState (runExceptT (oStmtScoped True s))
                      (St M.empty id) of
  Right os -> os
  Left er -> case er of
    WillCrash _ -> SExp fakeAnn (EApp fakeAnn TVoid "error" []) (SEmpty fakeAnn)
    EarlyReturn os -> os
