{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Latte.Frontend.Typecheck.Typechecker where

import Latte.Frontend.AST
import Latte.Frontend.Error
import Latte.Frontend.Typecheck.Types
import qualified Latte.Frontend.AST.ConstantOptimizer as CO

import Data.Functor
import Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import Control.Applicative hiding (empty)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens

import Prelude hiding (LT, GT, EQ, (<>))


raiseError :: Error -> Typechecker a
raiseError e = view loc >>= \a -> throwError $ ErrorPack $ pure (a, e)


raiseErrorAt :: MonadError ErrorPack m => Ann -> Error -> m a
raiseErrorAt a e = throwError $ ErrorPack $ pure (Just a, e)


raiseErrorNoLoc :: MonadError ErrorPack m => Error -> m a
raiseErrorNoLoc e = throwError $ ErrorPack $ pure (Nothing, e)


withAnn :: Ann -> Typechecker a -> Typechecker a
withAnn a = local (set loc (Just a))


withAnnOf :: HasAnn h Ann => h -> Typechecker a -> Typechecker a
withAnnOf h = withAnn (h^.ann)


getClassEntry :: ClassId -> Typechecker ClassEntry
getClassEntry i = views classEnv (M.lookup i) >>= \case
  Nothing -> raiseError $ UndefinedClass i
  Just c -> pure c


tcVar :: VarId -> Typechecker Type
tcVar v = (M.lookup v) <$> view definedVars >>= \case
  Nothing | v == "this" -> do
              view currentClass >>= \case
                Nothing -> raiseError ThisNotInClass
                Just c -> pure $ TClass c
  Nothing -> raiseError $ UndefinedVar v
  Just t -> pure t


tcFun :: FunId -> Typechecker (Type, [Type])
tcFun v = (M.lookup v) <$> view definedFuns >>= \case
  Nothing -> raiseError $ UndefinedFun v
  Just t -> pure t


checkAccess :: HasIdStr a String => ClassMemberAccess -> a -> ClassId -> Typechecker ()
checkAccess a x c = do
  myC <- view currentClass
  case a of
    Public -> pure ()
    Private -> when (myC /= Just c) $ raiseError $ BadPrivateAccess c (x^.idStr)
    Protected -> do
      maybe (pure False) (isSuper c) myC >>= \case
        False -> raiseError $ BadProtectedAccess c (x^.idStr)
        True -> pure ()


tcField :: ClassId -> FieldId -> Typechecker Type
tcField c m =
  let search sc = do
        ce <- getClassEntry sc
        case M.lookup m (ce^.fields) of
          Nothing -> case ce^.super of
            Nothing -> raiseError $ UndefinedField c m
            Just x -> search x
          Just (acc, x) -> do
            checkAccess acc m sc
            pure x
  in search c


tcMethod :: ClassId -> MethodId -> Typechecker (Type, [Type])
tcMethod c m =
  let search sc = do
        ce <- getClassEntry sc
        case M.lookup m (ce^.methods) of
          Nothing -> case ce^.super of
            Nothing -> raiseError $ UndefinedMethod c m
            Just x -> search x
          Just (acc, x) -> do
            checkAccess acc m sc
            pure x
  in search c


tcConstructor :: ClassId -> Maybe ConstructorId -> Typechecker [Type]
tcConstructor c m = do
  ce <- getClassEntry c
  case M.lookup m (ce^.constructors) of
    Nothing -> raiseError $ UndefinedConstructor c m
    Just (_, ts) -> pure ts


matchTypes :: Type -> Type -> Typechecker ()
matchTypes want got = case (want, got) of
  (TInt, TInt) -> pure ()
  (TVoid, TVoid) -> pure ()
  (TBool, TBool) -> pure ()
  (TString, TString) -> pure ()
  (TClass o1, TClass o2) -> matchClass o1 o2
  _ -> raiseError $ TypeMatch want got


isSuper :: ClassId -> ClassId -> Typechecker Bool
isSuper cs c =
  let search i =
        if cs == i then pure True
        else getClassEntry i >>= \ce -> case ce^.super of
          Nothing -> pure False
          Just i' -> search i'
  in search c


matchClass :: ClassId -> ClassId -> Typechecker ()
matchClass c1 c2 = isSuper c1 c2 >>= \case
  True -> pure ()
  False -> raiseError $ ClassMatch c1 c2


getSuper :: Typechecker ClassId
getSuper = do
  view currentClass >>= \case
    Nothing -> raiseError $ SuperNotInClass
    Just c -> getClassEntry c >>= \ce -> case ce^.super of
      Nothing -> raiseError $ NoSuperClass c
      Just x -> return x


assertType :: Type -> Expr 'Typed -> Typechecker ()
assertType t e = matchTypes t (getExprDec e)


tcType :: Type -> Typechecker ()
tcType = \case
  TClass cls -> do
    isDefined <- views classEnv (M.member cls)
    when (not isDefined) $ raiseError $ UndefinedClass cls
  _ -> return ()


tcOp :: AnyOp -> Type -> Type -> Typechecker Type
tcOp o l r =
  let tc t = matchTypes t r >> matchTypes t l
  in case o of
    Op (LT a)    -> withAnn a $ tc TInt $> TBool
    Op (LEQ a)   -> withAnn a $ tc TInt $> TBool
    Op (EQ a)    -> withAnn a $ matchTypes l r $> TBool
    Op (NEQ a)   -> withAnn a $ matchTypes l r $> TBool
    Op (GEQ a)   -> withAnn a $ tc TInt $> TBool
    Op (GT a)    -> withAnn a $ tc TInt $> TBool
    Op (Plus a)  -> withAnn a $ case (l, r) of
      (TInt, TInt) -> pure TInt
      (TString, TString) -> pure TString
      _ -> raiseError (OperatorTypeMatch o [(TInt, TInt), (TString, TString)] (l, r))
    Op (Minus a) -> withAnn a $ tc TInt $> TInt
    Op (Mult a)  -> withAnn a $ tc TInt $> TInt
    Op (Div a)   -> withAnn a $ tc TInt $> TInt
    Op (Mod a)   -> withAnn a $ tc TInt $> TInt
    Op (Or a)    -> withAnn a $ tc TBool $> TBool
    Op (And a)   -> withAnn a $ tc TBool $> TBool


tcUnOp :: UnOp -> Type -> Typechecker Type
tcUnOp o t = case o of
  Neg -> matchTypes TInt t >> return TInt
  Not -> matchTypes TBool t >> return TBool


tcExpr :: Expr 'Untyped -> Typechecker (Expr 'Typed)
tcExpr = \case
  ELit a () l -> withAnn a $ case l of
    LInt i -> pure $ ELit a TInt (LInt i)
    LString s -> pure $ ELit a TString (LString s)
    LBool b -> pure $ ELit a TBool (LBool b)
  EVar a () v -> withAnn a $ tcVar v >>= \t -> pure $ EVar a t v
  EApp a () fname as -> withAnn a $ do
    (rett, argst) <- tcFun fname
    typedArgs <- mapM tcExpr as
    when (length as /= length argst) $
      raiseError $ ArgNumFun fname (length argst) (length as)
    forM_ (zip argst typedArgs) $ \(expected, typed) ->
      assertType expected typed
    pure $ EApp a rett fname typedArgs
  EUnOp a () o v -> withAnn a $ do
    vt <- tcExpr v
    rt <- tcUnOp o (getExprDec vt)
    pure $ EUnOp a rt o vt
  EOp a () o l r -> withAnn a $ do
    tl <- tcExpr l
    tr <- tcExpr r
    tres <- tcOp o (getExprDec tl) (getExprDec tr)
    pure $ EOp a tres o tl tr
  EProj apr () (ESuper a ()) i -> withAnn a $ do
    s <- getSuper
    t <- tcField s i
    pure $ EProj apr t (ESuper a (TClass s)) i
  EProj a () e i -> withAnn a $ do
    et <- tcExpr e
    case getExprDec et of
      TClass c -> do
        t <- tcField c i
        pure $ EProj a t et i
      t -> raiseError $ NotAClass t
  EMApp apr () (ESuper a ()) i as -> withAnn a $ do
    s <- getSuper
    (rt, argst) <- tcMethod s i
    typedArgs <- mapM tcExpr as
    when (length as /= length argst) $
      raiseError $ ArgNumMethod s i (length argst) (length as)
    forM_ (zip argst typedArgs) $ \(expected, typed) ->
      assertType expected typed
    pure $ EMApp apr rt (ESuper a (TClass s)) i typedArgs
  EMApp a () e i as -> withAnn a $ do
    et <- tcExpr e
    case getExprDec et of
      TClass c -> do
        (rt, argst) <- tcMethod c i
        typedArgs <- mapM tcExpr as
        when (length as /= length argst) $
          raiseError $ ArgNumMethod c i (length argst) (length as)
        forM_ (zip argst typedArgs) $ \(expected, typed) ->
          assertType expected typed
        pure $ EMApp a rt et i typedArgs
      t -> raiseError $ NotAClass t
  ENew a () c i as -> withAnn a $ do
    argst <- tcConstructor c i
    typedArgs <- mapM tcExpr as
    when (length as /= length argst) $
      raiseError $ ArgNumConstructor c i (length argst) (length as)
    forM_ (zip argst typedArgs) $ \(expected, typed) ->
      assertType expected typed
    pure $ ENew a (TClass c) c i typedArgs
  ESuper a () -> withAnn a $ raiseError BadSuperUse


currentRetType :: Typechecker Type
currentRetType = maybe (error "fun env not in a funtion") id <$>
  view retType


newScope :: Typechecker a -> Typechecker a
newScope = local (set currentScopeVars S.empty)


tcStmt :: Stmt 'Untyped -> Typechecker (Stmt 'Typed)
tcStmt = \case
  SDecl a t v k -> withAnn a $ do
    tcType t
    vars <- view currentScopeVars
    void $ flip runStateT vars $ forM_ v $ \(var, _) -> do
      myvars <- get
      when (S.member var myvars) (lift $ raiseError $ DuplicateVar var)
      modify (S.insert var)
    oldenv <- view definedVars
    (newEnv, tv) <-
      foldrM ( \(var, mval) (penv, pdecls) -> do
                 case mval of
                   Nothing -> return (M.insert var t penv, (var, Nothing):pdecls)
                   Just e -> do
                     et <- tcExpr e
                     assertType t et
                     return (M.insert var t penv, (var, Just et):pdecls)
             ) (oldenv, []) v
    local ( set definedVars newEnv .
            over currentScopeVars (S.union $ S.fromList (fmap fst v))) $
      SDecl a t tv <$> tcStmt k
  SAssg a v e k -> withAnn a $ do
    et <- tcExpr e
    vt <- tcVar v
    assertType vt et
    SAssg a v et <$> tcStmt k
  SFieldAssg a b f e k -> withAnn a $ do
    bt <- tcExpr b
    et <- tcExpr e
    case getExprDec bt of
      TClass c -> do
        t <- tcField c f
        assertType t et
        SFieldAssg a bt f et <$> tcStmt k
      t -> raiseError $ NotAClass t
  SIncr a v k -> withAnn a $ do
    vt <- tcVar v
    matchTypes TInt vt
    SIncr a v <$> tcStmt k
  SDecr a v k -> withAnn a $ do
    vt <- tcVar v
    matchTypes TInt vt
    SDecr a v <$> tcStmt k
  SCond a c t k -> withAnn a $ do
    ct <- tcExpr c
    assertType TBool ct
    tt <- newScope $ tcStmt t
    SCond a ct tt <$> tcStmt k
  SCondElse a c t e k -> withAnn a $ do
    ct <- tcExpr c
    assertType TBool ct
    tt <- newScope $ tcStmt t
    et <- newScope $ tcStmt e
    SCondElse a ct tt et <$> tcStmt k
  SWhile a c b k -> withAnn a $ do
    ct <- tcExpr c
    assertType TBool ct
    bt <- newScope $ tcStmt b
    SWhile a ct bt <$> tcStmt k
  SExp a e k -> withAnn a $ do
    et <- tcExpr e
    SExp a et <$> tcStmt k
  SRet a e k -> withAnn a $ do
    et <- tcExpr e
    rt <- currentRetType
    assertType rt et
    SRet a et <$> tcStmt k
  SVRet a k -> withAnn a $ do
    rt <- currentRetType
    matchTypes rt TVoid
    SVRet a <$> tcStmt k
  SBlock a b k -> withAnn a $
    SBlock a <$> (newScope $ tcStmt b) <*> tcStmt k
  SEmpty a -> withAnn a $ pure $ SEmpty a


isReturning :: Stmt a -> Bool
isReturning = \case
  SDecl _a _t _decls k -> isReturning k
  SAssg _a _v _e k -> isReturning k
  SFieldAssg _a _ee _v _e k -> isReturning k
  SIncr _a _v k -> isReturning k
  SDecr _a _v k -> isReturning k
  SCond _a _c _t k -> isReturning k
  SCondElse _a _c t e k ->
    (isReturning t && isReturning e) || isReturning k
  SWhile _a (ELit _ _ (LBool True)) b _k -> isReturning b
  SWhile _a _c _b k -> isReturning k
  SExp _a (EApp _ _ (FunId "error") _) _k -> True
  SExp _a _e k -> isReturning k
  SRet _a _e _ -> True
  SVRet _a _ -> True
  SBlock _a b k ->
    isReturning b || isReturning k
  SEmpty _ -> False


buildClassEntry :: ClassDef 'Untyped -> ClassEntry
buildClassEntry c = ClassEntry
  { _ceSuper = c ^. super
  , _ceFields = M.fromList
                [(i, t) | (CMField f) <- c^.body
                        , let t = (f^.access, f^.ty)
                        , (i, _) <- NE.toList (f^.assignments)
                        ]
  , _ceMethods = M.fromList
                 [(i, (acc, (rt, as))) | (CMMethod m) <- c^.body
                          , let as = fmap (^.ty) $ m^.args
                                rt = m^.retType
                                i = m^.name
                                acc = m^.access
                          ]
  , _ceConstructors = M.fromList
    [(i, (acc, as)) | (CMConstructor co) <- c^.body
             , let as = fmap (^.ty) $ co^.args
                   i = co^.name
                   acc = co^.access
             ]
  }


buildInitialEnv :: [TopDef 'Untyped] -> TopTypechecker TypecheckerEnv
buildInitialEnv defs = do
  when (null [() | TDFun fdef <- defs, fdef^.name == "main"]) $
    raiseErrorNoLoc NoMain
  env <- foldM (\prev d ->
           case d of
             TDFun fdef -> do
               when (fdef^.name == "main" && (fdef^.retType /= TInt || not (null $ fdef^.args))) $
                 raiseErrorAt (fdef^.ann) MainType
               when (fdef^.name `elem` (M.keys $ prev^.definedFuns)) $
                 raiseErrorAt (fdef^.ann) $ DuplicateFun (fdef^.name)
               pure $
                 over definedFuns (M.insert (fdef^.name) (fdef^.retType, fmap (^.ty) (fdef^.args))) prev
             TDClass cd -> do
               when (cd^.name `elem` (M.keys $ prev^.classEnv)) $
                 raiseErrorAt (cd^.ann) $ DuplicateClass (cd^.name)
               let ce = buildClassEntry cd
                   checkDups :: (Ord s)
                             => [(Ann, x)] -> (x -> s) -> (s -> Error) -> TopTypechecker ()
                   checkDups vals getname err = flip evalStateT S.empty $ forM_ vals $ \(a, f) -> do
                     jeb <- gets $ S.member (getname f)
                     when jeb $ raiseErrorAt a $ err (getname f)
                     modify $ S.insert (getname f)
               checkDups [(fd^.ann, f) | CMField fd <- cd^.body, (f, _) <- NE.toList $ fd^.assignments ] id (DuplicateField (cd^.name))
               checkDups [(m^.ann, m) | CMMethod m <- cd^.body ] (^.name) (DuplicateMethod (cd^.name))
               checkDups [(c^.ann, c) | CMConstructor c <- cd^.body ] (^.name) (DuplicateConstructor (cd^.name))
               pure $ over classEnv (M.insert (cd^.name) ce) prev
        ) initialEnv defs
  forM_ [cd | TDClass cd <- defs] $ \cd ->
    forM_ (cd^.super) $ \sup ->
      when (not $ M.member sup (env^.classEnv))
        (raiseErrorAt (cd^.ann) (UndefinedClass sup))
  return env


tcTopDef :: TopDef 'Untyped -> Typechecker (TopDef 'Typed)
tcTopDef = \case
  TDFun fdef -> do
    tcType (fdef^.retType)
    forM_ (fmap (^.ty) $ fdef^.args) tcType
    let addArgEnv =
          flip M.union (M.fromList $ fmap (\a -> (a^.name, a^.ty)) (fdef^.args))
    let bodyEnv = (over definedVars addArgEnv . set retType (Just $ fdef^.retType))
    tbody <- do
      tb <- CO.optimizeBody (fdef^.args) <$> local bodyEnv (tcStmt $ fdef^.body)
      when (not $ (fdef^.retType == TVoid) || (fdef^.name=="main") || isReturning tb) $
        raiseErrorAt (fdef^.ann) NoReturn
      return tb
    pure $ TDFun $ FunDef (fdef^.ann) (fdef^.retType) (fdef^.name) (fdef^.args) tbody
  TDClass cdef -> do
    let this = Arg { _argAnn = fakeAnn, _argName = "this", _argTy = TClass (cdef^.name) }

        tcMember :: ClassMember 'Untyped -> Typechecker (ClassMember 'Typed)
        tcMember = \case
          CMMethod mdef -> do
            tcType $ mdef^.retType
            forM_ (fmap (^.ty) $ mdef^.args) tcType
            let addArgEnv =
                  flip M.union (M.fromList $ fmap (\a -> (a^.name, a^.ty)) (mdef^.args))
            let setBodyEnv = over definedVars addArgEnv .
                             set retType (Just $ mdef^.retType) .
                             set currentScopeName (Just $ mdef^.name.idStr) .
                             set currentClass (Just $ cdef^.name)
            tbody <- case mdef^.body of
              Nothing -> pure Nothing
              Just b -> Just <$>
                do tb <- CO.optimizeBody (this : mdef^.args) <$> local setBodyEnv (tcStmt b)
                   when (not $ (mdef^.retType == TVoid) || isReturning tb) $
                     raiseErrorAt (mdef^.ann) NoReturn
                   return tb
            pure $ CMMethod $ Method
              { _methodAnn = mdef^.ann
              , _methodAccess = mdef^.access
              , _methodPlace = mdef^.place
              , _methodRetType = mdef^.retType
              , _methodName = mdef^.name
              , _methodArgs = mdef^.args
              , _methodBody = tbody
              }
          CMField fdef -> do
            tcType $ fdef^.ty
            tassgs <- forM (fdef^.assignments) $ \(i, mv) -> do
              case mv of
                Nothing -> return (i, Nothing)
                Just v -> do
                  let setBodyEnv = set currentClass (Just $ cdef^.name)
                  vt <- local setBodyEnv (tcExpr v)
                  assertType (fdef^.ty) vt
                  return (i, Just vt)
            pure $ CMField $ Field
              { _fieldAnn = fdef^.ann
              , _fieldAccess = fdef^.access
              , _fieldPlace = fdef^.place
              , _fieldTy = fdef^.ty
              , _fieldAssignments = tassgs
              }
          CMConstructor codef -> do
            forM_ (codef^.args) $ \a -> withAnnOf a $ do
              when (a^.name == "this") $
                raiseError $ ThisArgName
              tcType (a^.ty)
            let addArgEnv =
                  flip M.union (M.fromList $ fmap (\a -> (a^.name, a^.ty)) (codef^.args))
            let setBodyEnv =
                  over definedVars addArgEnv .
                  set retType (Just $ TClass (cdef^.name)) .
                  set currentScopeName (fmap (^.idStr) (codef^.name) <|> Just "unnamed constructor") .
                  set currentClass (Just $ cdef^.name)
            tbody <- CO.optimizeBody (this : codef^.args) <$>
                     local setBodyEnv (tcStmt (codef^.body))
            pure $ CMConstructor $ Constructor
              { _constructorAnn    = codef^.ann
              , _constructorAccess = codef^.access
              , _constructorName   = codef^.name
              , _constructorArgs   = codef^.args
              , _constructorBody   = tbody
              }
    members <- mapM tcMember (cdef^.body)
    pure $ TDClass $ ClassDef (cdef^.ann) (cdef^.name) (cdef^.super) members


tcProgram :: Program 'Untyped -> Typechecker (Program 'Typed)
tcProgram (Program defs) = Program <$> mapM tcTopDef defs


typecheck :: Program 'Untyped -> Either ErrorPack (ClassEnv, Program 'Typed)
typecheck p@(Program defs) = do
  env <- runExcept $ buildInitialEnv defs
  flip runReader env . runExceptT $ ((,) (env^.classEnv) <$> tcProgram p)
