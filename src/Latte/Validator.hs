{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Latte.Validator where

import Latte.Types.AST
import Latte.Types.Latte
import qualified Latte.Types.Syntax as S
import Latte.Error
import Latte.StdLib

import Data.Functor
import qualified Data.Map as M
import Data.Map(Map)
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
  ExceptT String (Reader ValidatorEnv)

type VarEnv = Map Id Type
type FunEnv = Map Id (Type, [Type])


data ValidatorEnv = ValidatorEnv
  { _veDefinedVars :: VarEnv
  , _veDefinedFuns :: FunEnv
  , _veLoc         :: Ann
  , _veRetType     :: Maybe Type
  -- , _veTopDefCont  :: forall a. Validator a
  }

-- data ValidatorState = ValidatorState
--   { _vsErrors      :: [String]
--   }


makeLenses ''ValidatorEnv
-- makeLenses ''ValidatorState


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

tcStmt :: Stmt (Expr ()) -> Validator (Stmt (Expr Type))
tcStmt = \case
  SDecl ann t decls k -> do
    newDecls <- forM decls $ \(i, me) -> do
      mte <- forM me $ \e -> do
        te <- tcExpr e
        assertType t te
        pure te
      pure (i, mte)
    let envExtension = M.fromList $ zip (NE.toList $ fmap fst decls) (repeat t)
    newEnv <- M.union envExtension <$> view veDefinedVars
    SDecl ann t newDecls <$> local (set veDefinedVars newEnv) (tcStmt k)
  SAssg ann v e k -> do
    et <- tcExpr e
    vt <- tcVar v
    assertType vt et
    SAssg ann v et <$> tcStmt k
  SIncr ann v k -> do
    vt <- tcVar v
    matchTypes TInt vt
    SIncr ann v <$> tcStmt k
  SDecr ann v k -> do
    vt <- tcVar v
    matchTypes TInt vt
    SDecr ann v <$> tcStmt k
  SCond ann c t k -> do
    ct <- tcExpr c
    assertType TBool ct
    tt <- tcStmt t
    SCond ann ct tt <$> tcStmt k
  SCondElse ann c t e k -> do
    ct <- tcExpr c
    assertType TBool ct
    tt <- tcStmt t
    et <- tcStmt e
    SCondElse ann ct tt et <$> tcStmt k
  SWhile ann c b k -> do
    ct <- tcExpr c
    assertType TBool ct
    bt <- tcStmt b
    SCond ann ct bt <$> tcStmt k
  SExp ann e k -> do
    et <- tcExpr e
    SExp ann et <$> tcStmt k
  SRet ann e -> do
    et <- tcExpr e
    rt <- retType
    assertType rt et
    pure $ SRet ann et
  SVRet ann -> do
    rt <- retType
    matchTypes rt TVoid
    pure $ SVRet ann
  SBlock ann b k -> do
    bt <- tcStmt b
    kt <- tcStmt k
    pure $ SBlock ann bt kt
  SEmpty -> pure SEmpty


isReturning :: Stmt a -> Bool
isReturning = \case
  SDecl ann t decls k -> isReturning k
  SAssg ann v e k -> isReturning k
  SIncr ann v k -> isReturning k
  SDecr ann v k -> isReturning k
  SCond ann c t k -> isReturning k
  SCondElse ann c t e k ->
    (isReturning t && isReturning e) || isReturning k
  SWhile ann c b k -> isReturning k
  SExp ann e k -> isReturning k
  SRet ann e -> True
  SVRet ann -> True
  SBlock ann b k ->
    isReturning b || isReturning k
  SEmpty -> False

buildGlobalEnv :: [TopDef ()] -> (VarEnv, FunEnv)
buildGlobalEnv = foldl (\(pv, pf) d -> case d of
                           FunDef _ rt fn args _ ->
                             (pv, M.insert fn (rt, fmap (\(Arg _ t _) -> t) args) pf)
                           ) (M.empty, stdfenv)

tcTopDef :: TopDef () -> Validator (TopDef Type)
tcTopDef = \case
  FunDef ann retType fname args body -> do
    let addArgEnv = flip M.union (M.fromList $ fmap (\(Arg _ t n) -> (n, t)) args)
    when (not $ isReturning body) $ throwError noReturn
    tbody <- local (over veDefinedVars addArgEnv . set veRetType (Just retType)) (tcStmt body)
    pure $ FunDef ann retType fname args tbody

tcProgram :: Program () -> Either String (Program Type)
tcProgram (Program defs) =
  let (varEnv, funEnv) = buildGlobalEnv defs
  in runReader (runExceptT (Program <$> mapM tcTopDef defs))
     (ValidatorEnv varEnv funEnv (Ann "toplevel" 0 0) Nothing)
