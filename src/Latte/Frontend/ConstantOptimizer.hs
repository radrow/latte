{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module Latte.Frontend.ConstantOptimizer where

import Latte.Frontend.AST
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (LT, GT, EQ)

type ValMap = M.Map Int Lit

data St = St
  { _stValMap :: ValMap
  , _stEarlyStop :: Bool
  , _stSupply :: !Int
  }
makeLensesWith abbreviatedFields ''St

type NameMap = M.Map VarId Int
type Env = NameMap

type Optimizer = ReaderT Env (State St)

pattern LI :: Integer -> Expr 'Typed
pattern LI i <- ELit _ _ (LInt i)
pattern LB :: Bool -> Expr 'Typed
pattern LB i <- ELit _ _ (LBool i)

getVar :: VarId -> Optimizer (Maybe Lit)
getVar v = ask >>= \rm -> uses valMap (M.lookup (rm `idOf` v))

idOf :: NameMap -> VarId -> Int
idOf nm v = case M.lookup v nm of
  Nothing -> error $ "wtf, no such name " ++ v^.idStr
  Just x -> x

updateVar :: VarId -> Lit -> Optimizer ()
updateVar v l = do
  rm <- ask
  modify $ over valMap (M.insert (rm `idOf` v) l)

assignVar :: VarId -> Expr 'Typed -> Optimizer ()
assignVar v e = do
  oExpr e >>= \case
    ELit _ _ x -> updateVar v x
    _ -> pure ()

unsetVars :: S.Set VarId -> Optimizer ()
unsetVars vars = do
  rm <- ask
  let keep = S.map (idOf rm) $ M.keysSet rm S.\\ vars
  modify $ over valMap $ flip M.restrictKeys keep

getSup :: Optimizer Int
getSup = use supply <* modify (over supply (+1))

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
  EApp _ _ fname [] | fname == "error" -> stop ex
  EApp _ _ _fname _as -> return ex
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
      _ -> return $ EOp a t (Op o) ll rr
  _ -> return ex

varsScoped :: Stmt 'Typed -> S.Set VarId
varsScoped = \case
  SDecl _a _t v k -> S.insert v $ varsScoped k
  SAssg _a _v _e k -> varsScoped k
  SFieldAssg _a _b _f _e k -> varsScoped k
  SIncr _a _v k -> varsScoped k
  SDecr _a _v k -> varsScoped k
  SCond _a _c _t k -> varsScoped k
  SCondElse _a _c _t _e k -> varsScoped k
  SWhile _a _c _b k -> varsScoped k
  SExp _a _e k -> varsScoped k
  SRet _a _e _k -> S.empty
  SVRet _a _k -> S.empty
  SBlock _a _b k -> varsScoped k
  SEmpty _a -> S.empty


varsUpdated :: Stmt 'Typed -> S.Set VarId
varsUpdated = \case
  SDecl _a _t v k -> S.delete v $ varsUpdated k
  SAssg _a v _e k -> S.insert v $ varsUpdated k
  SFieldAssg _a _b _f _e k -> varsUpdated k
  SIncr _a v k -> S.insert v $ varsUpdated k
  SDecr _a v k -> S.insert v $ varsUpdated k
  SCond _a _c t k -> S.union (varsUpdated t) (varsUpdated k)
  SCondElse _a _c t e k -> S.unions [varsUpdated t, varsUpdated e, varsUpdated k]
  SWhile _a _c b k -> S.union (varsUpdated b) (varsUpdated k)
  SExp _a _e k -> varsUpdated k
  SRet _a _e _k -> S.empty
  SVRet _a _k -> S.empty
  SBlock _a b k -> S.union (varsUpdated b) (varsUpdated k)
  SEmpty _a -> S.empty


oStmtScoped :: Stmt 'Typed -> Optimizer (Stmt 'Typed)
oStmtScoped ob = do
  backupStop <- use earlyStop
  modify $ set earlyStop False
  res <- oStmt ob
  modify $ set earlyStop backupStop
  return res

continue :: (Stmt 'Typed -> Stmt 'Typed) -> Stmt 'Typed -> Optimizer (Stmt 'Typed)
continue b k = do
  es <- use earlyStop
  if es then return (b (SEmpty fakeAnn)) else b <$> oStmt k

oStmt :: Stmt 'Typed -> Optimizer (Stmt 'Typed)
oStmt s = case s of
  SDecl a t v k -> do
    uniqV <- getSup
    local (M.insert v uniqV) $ continue (SDecl a t v) k
  SAssg _a v (EVar _ _ vv) k | v == vv -> oStmt k
  SAssg a v e k -> do
    oe <- oExpr e
    assignVar v oe
    continue (SAssg a v oe) k
  SFieldAssg a b f e k -> do
    oe <- oExpr e
    continue (SFieldAssg a b f oe) k
  SIncr a v k -> do
    getVar v >>= \case
      Just (LInt i) -> updateVar v (LInt $ i+1)
      _ -> pure ()
    continue (SIncr a v) k
  SDecr a v k -> do
    getVar v >>= \case
      Just (LInt i) -> updateVar v (LInt $ i-1)
      _ -> pure ()
    continue (SDecr a v) k
  SCond a c t k -> do
    oc <- oExpr c
    case oc of
      LB True ->
        oStmt $ SBlock a t k
      LB False ->
        oStmt k
      _ -> do
        ot <- oStmtScoped t
        let ut = varsUpdated t
        unsetVars ut
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
        let ut = varsUpdated t
            ue = varsUpdated e
        unsetVars (S.union ut ue)
        continue (SCondElse a oc ot oe) k
  SWhile a c b k -> do
    let ub = varsUpdated b
    backup <- use valMap
    unsetVars ub
    oc <- oExpr c
    case oc of
      LB True ->
        stop (SWhile a c b (SEmpty fakeAnn))
      LB False -> do
        modify $ set valMap backup
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

optimizeBody :: [Arg] -> Stmt 'Typed -> Stmt 'Typed
optimizeBody as s = evalState (runReaderT (oStmt s) (M.fromList $ zip (fmap (^.name) as) [-1, -2..]))
  (St M.empty False 0)
