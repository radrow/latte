{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Latte.Validator where

import Latte.Types.AST
import Latte.Types.Latte
import qualified Latte.Types.Syntax as S
import Latte.Types.Syntax(Stmt(..))
import Latte.Error

import Data.Functor
import Data.Map as M
import qualified Data.List.NonEmpty as NE
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Lens

type LangError = [String]


-- type Validator =
--   StateT ValidatorState (ReaderT ValidatorEnv (Cont ValidatorState))
type Validator =
  ExceptT String (StateT ValidatorState (Reader ValidatorEnv))

type VarEnv = Map Id Type
type FunEnv = Map Id (Type, [Type])


data ValidatorEnv = ValidatorEnv
  { _veDefinedVars :: VarEnv
  , _veDefinedFuns :: FunEnv
  , _veLoc         :: Ann
  , _veRetType     :: Maybe Type
  -- , _veTopDefCont  :: forall a. Validator a
  }

data ValidatorState = ValidatorState
  { _vsErrors      :: [String]
  }


makeLenses ''ValidatorEnv
makeLenses ''ValidatorState


withAnn :: HasAnn a => (a -> Validator b) -> a -> Validator b
withAnn con ob = local (set veLoc (getAnn ob)) (con ob)


-- withCont :: Validator (Maybe a) -> Validator (Maybe a)
-- withCont act = callCC (\k -> local (set veTopDefCont (k Nothing)) act)


-- abortTopDef :: Validator ()
-- abortTopDef = join $ asks (^. veTopDefCont)


-- pushError :: String -> Validator ()
-- pushError e = modify (over vsErrors (e:)) >> abortTopDef


tcVar :: Id -> Validator Type
tcVar v = (M.lookup v) <$> view veDefinedVars >>= \case
  Nothing -> throwError $ undefinedVar v
  Just t -> pure t

tcFun :: Id -> Validator (Type, [Type])
tcFun v = (M.lookup v) <$> view veDefinedFuns >>= \case
  Nothing -> throwError $ undefinedVar v
  Just t -> pure t

matchTypes :: Type -> Type -> Validator ()
matchTypes a b = if a == b then pure () else throwError $ typeMatchError a b

assertType :: Type -> Expr Type -> Validator ()
assertType expected typed =
  matchTypes expected (getExprDec typed)

tcOp :: Op -> Type -> Type -> Validator Type
tcOp o l r =
  let tc t = matchTypes t r >> matchTypes t l
  in case o of
    Op (S.LT _)    -> tc TInt $> TBool
    Op (S.LEQ _)   -> tc TInt $> TBool
    Op (S.EQ _)    -> matchTypes l r $> TBool
    Op (S.NEQ _)   -> matchTypes l r $> TBool
    Op (S.GEQ _)   -> tc TInt $> TBool
    Op (S.GT _)    -> tc TInt $> TBool
    Op (S.Plus _)  -> (tc TInt $> TInt) <|> (tc TString $> TString)
    Op (S.Minus _) -> tc TInt $> TInt
    Op (S.Mult _)  -> tc TInt $> TInt
    Op (S.Div _)   -> tc TInt $> TInt
    Op (S.Mod _)   -> tc TInt $> TInt
    Op (S.Or _)    -> tc TBool $> TBool
    Op (S.And _)   -> tc TBool $> TBool


tcExpr :: Expr () -> Validator (Expr Type)
tcExpr = \case
  ELit ann () l -> case l of
    LInt i -> pure $ ELit ann TInt (LInt i)
    LString s -> pure $ ELit ann TString (LString s)
    LBool b -> pure $ ELit ann TBool (LBool b)
  EVar ann () v -> tcVar v >>= \t -> pure $ EVar ann t v
  EApp ann () fname args -> do
    (rett, argst) <- tcFun fname
    typedArgs <- mapM tcExpr args
    forM_ (zip argst typedArgs) $ \(expected, typed) ->
      assertType expected typed
    pure $ EApp ann rett fname typedArgs
  ENeg ann () v -> do
    vt <- tcExpr v
    assertType TInt vt
    pure $ ENeg ann TInt vt
  ENot ann () v -> do
    vt <- tcExpr v
    assertType TBool vt
    pure $ ENot ann TBool vt
  EOp ann () o l r -> do
    tl <- tcExpr l
    tr <- tcExpr r
    tres <- tcOp o (getExprDec tl) (getExprDec tr)
    pure $ EOp ann tres o tl tr

retType :: Validator Type
retType = maybe (error "fun env not in a funtion") id <$>
  view veRetType

sameEnv :: Stmt (Expr Type) -> Validator (Stmt (Expr Type), VarEnv)
sameEnv s = (,) s <$> view veDefinedVars

tcStmt :: Stmt (Expr ()) -> Validator (Stmt (Expr Type), VarEnv)
tcStmt = \case
  SDecl ann t decls -> do
    newDecls <- forM decls $ \(i, me) -> do
      mte <- forM me $ \e -> do
        te <- tcExpr e
        assertType t te
        pure te
      pure (i, mte)
    let envExtension = M.fromList $ zip (NE.toList $ fmap fst decls) (repeat t)
    newEnv <- M.union envExtension <$> view veDefinedVars
    pure (SDecl ann t newDecls, newEnv)
  SAssg ann v e -> do
    et <- tcExpr e
    vt <- tcVar v
    assertType vt et
    sameEnv $ SAssg ann v et
  SIncr ann v -> do
    vt <- tcVar v
    matchTypes TInt vt
    sameEnv $ SIncr ann v
  SDecr ann v -> do
    vt <- tcVar v
    matchTypes TInt vt
    sameEnv $ SDecr ann v
  SCond ann c t -> do
    ct <- tcExpr c
    assertType TBool ct
    (tt, _) <- tcStmt t
    sameEnv $ SCond ann ct tt
  SCondElse ann c t e -> do
    ct <- tcExpr c
    assertType TBool ct
    (tt, _) <- tcStmt t
    (et, _) <- tcStmt e
    sameEnv $ SCondElse ann ct tt et
  SWhile ann c b -> do
    ct <- tcExpr c
    assertType TBool ct
    (bt, _) <- tcStmt b
    sameEnv $ SCond ann ct bt
  SExp ann e -> do
    et <- tcExpr e
    sameEnv $ SExp ann et
  SRet ann e -> do
    et <- tcExpr e
    rt <- retType
    assertType rt et
    sameEnv $ SRet ann et
  SVRet ann -> do
    rt <- retType
    matchTypes rt TVoid
    sameEnv $ SVRet ann
  SBlock ann stmts -> do
    env <- view veDefinedVars
    let step :: (Stmt (Expr Type) -> Stmt (Expr Type), VarEnv) ->
                (Stmt (Expr ())) ->
                (Stmt (Expr Type) -> Stmt (Expr Type), VarEnv)
        step (prevCont, prevEnv) stmt = do
          (stmtt, newEnv) <-
            local (over veDefinedVars (const prevEnv)) (tcStmt stmt)
          pure (prevCont . (stmtt:), newEnv)
    (stmtstCont, newEnv) <- foldM step (id, env) stmts
    pure (SBlock ann (stmtstCont []), newEnv)
  SEmpty ann -> sameEnv $ SEmpty ann
