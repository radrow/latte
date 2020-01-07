{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Latte.Frontend.Typechecker where

import Latte.Frontend.AST
import Latte.Frontend.Error

import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map(Map)
import qualified Data.List.NonEmpty as NE
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens

import Prelude hiding (LT, GT, EQ)


type LangError = [String]

type Typechecker =
  ReaderT TypecheckerEnv (Except String)

type VarEnv   = Map Id Type
type FunEnv   = Map Id (Type, [Type])
type ConEnv   = Map (Maybe Id) [Type]
type ClassEnv = Map Id ClassEntry
data ClassEntry = ClassEntry
  { _ceSuper        :: Maybe Id
  , _ceFields       :: VarEnv
  , _ceMethods      :: FunEnv
  , _ceConstructors :: ConEnv
  }


data TypecheckerEnv = TypecheckerEnv
  { _teDefinedVars      :: VarEnv
  , _teDefinedFuns      :: FunEnv
  , _teCurrentFun       :: Maybe Id
  , _teCurrentClass     :: Maybe Id
  , _teClassEnv         :: ClassEnv
  , _teLoc              :: Ann
  , _teRetType          :: Maybe Type
  , _teCurrentScopeVars :: S.Set Id
  }

initialEnv :: TypecheckerEnv
initialEnv = TypecheckerEnv
  { _teDefinedVars      = M.singleton "void" TVoid
  , _teDefinedFuns      = M.fromList $
    [ ("printInt", (TVoid, [TInt]))
    , ("printString", (TVoid, [TString]))
    , ("readInt", (TInt, []))
    , ("readString", (TString, []))
    , ("error", (TVoid, []))
    ]
  , _teCurrentFun       = Nothing
  , _teCurrentClass     = Nothing
  , _teClassEnv         = M.empty
  , _teLoc              = fakeAnn
  , _teRetType          = Nothing
  , _teCurrentScopeVars = S.empty
  }

makeLensesWith abbreviatedFields ''TypecheckerEnv
makeLensesWith abbreviatedFields ''ClassEntry


getClassEntry :: Id -> Typechecker ClassEntry
getClassEntry i = views classEnv (M.lookup i) >>= \case
  Nothing -> throwError $ undefinedClass i
  Just c -> pure c


tcVar :: Id -> Typechecker Type
tcVar v = (M.lookup v) <$> view definedVars >>= \case
  Nothing | v == Id "this" -> do
              view currentClass >>= \case
                Nothing -> throwError notInClass
                Just c -> pure $ TClass c
  Nothing -> throwError $ undefinedVar v
  Just t -> pure t


tcFun :: Id -> Typechecker (Type, [Type])
tcFun v = (M.lookup v) <$> view definedFuns >>= \case
  Nothing -> throwError $ undefinedVar v
  Just t -> pure t

tcField :: Id -> Id -> Typechecker Type
tcField c m =
  let search sc = do
        ce <- getClassEntry sc
        case M.lookup m (ce^.fields) of
          Nothing -> case ce^.super of
            Nothing -> throwError $ noSuchField c m
            Just x -> search x
          Just x -> pure x
  in search c

tcMethod :: Id -> Id -> Typechecker (Type, [Type])
tcMethod c m =
  let search sc = do
        ce <- getClassEntry sc
        case M.lookup m (ce^.methods) of
          Nothing -> case ce^.super of
            Nothing -> throwError $ noSuchMethod c m
            Just x -> search x
          Just x -> pure x
  in search c

tcConstructor :: Id -> Maybe Id -> Typechecker [Type]
tcConstructor c m = do
  ce <- getClassEntry c
  case M.lookup m (ce^.constructors) of
    Nothing -> throwError $ noSuchConstructor c m
    Just ts -> pure ts

matchTypes :: Type -> Type -> Typechecker ()
matchTypes a b = case (a, b) of
  (TInt, TInt) -> pure ()
  (TVoid, TVoid) -> pure ()
  (TBool, TBool) -> pure ()
  (TString, TString) -> pure ()
  (TClass o1, TClass o2) -> matchClass o1 o2
  _ -> throwError $ typeMatchError a b


matchClass :: Id -> Id -> Typechecker ()
matchClass c1 c2 =
  let search i1 i2 =
        if i1 == i2 then pure True
        else getClassEntry i2 >>= \ce -> case ce^.super of
          Nothing -> pure False
          Just i2' -> search i1 i2'
  in search c1 c2 >>= \case
    True -> pure ()
    False -> throwError $ classMatchError c1 c2

assertType :: Type -> Expr 'Typed -> Typechecker ()
assertType t e = matchTypes t (getExprDec e)


tcOp :: AnyOp -> Type -> Type -> Typechecker Type
tcOp o l r =
  let tc t = matchTypes t r >> matchTypes t l
  in case o of
    Op (LT _)    -> tc TInt $> TBool
    Op (LEQ _)   -> tc TInt $> TBool
    Op (EQ _)    -> matchTypes l r $> TBool
    Op (NEQ _)   -> matchTypes l r $> TBool
    Op (GEQ _)   -> tc TInt $> TBool
    Op (GT _)    -> tc TInt $> TBool
    Op (Plus _)  -> (tc TInt $> TInt) <|> (tc TString $> TString)
    Op (Minus _) -> tc TInt $> TInt
    Op (Mult _)  -> tc TInt $> TInt
    Op (Div _)   -> tc TInt $> TInt
    Op (Mod _)   -> tc TInt $> TInt
    Op (Or _)    -> tc TBool $> TBool
    Op (And _)   -> tc TBool $> TBool


tcUnOp :: UnOp -> Type -> Typechecker Type
tcUnOp o t = case o of
  Neg -> matchTypes TInt t >> return TInt
  Not -> matchTypes TBool t >> return TBool


tcExpr :: Expr 'Untyped -> Typechecker (Expr 'Typed)
tcExpr = \case
  ELit a () l -> case l of
    LInt i -> pure $ ELit a TInt (LInt i)
    LString s -> pure $ ELit a TString (LString s)
    LBool b -> pure $ ELit a TBool (LBool b)
  EVar a () v -> tcVar v >>= \t -> pure $ EVar a t v
  EApp a () fname as -> do
    (rett, argst) <- tcFun fname
    typedArgs <- mapM tcExpr as
    when (length as /= length argst) $
      throwError $ argNumMismatch (length argst) (length as)
    forM_ (zip argst typedArgs) $ \(expected, typed) ->
      assertType expected typed
    pure $ EApp a rett fname typedArgs
  EUnOp a () o v -> do
    vt <- tcExpr v
    rt <- tcUnOp o (getExprDec vt)
    pure $ EUnOp a rt o vt
  EOp a () o l r -> do
    tl <- tcExpr l
    tr <- tcExpr r
    tres <- tcOp o (getExprDec tl) (getExprDec tr)
    pure $ EOp a tres o tl tr
  EProj a () e i -> do
    et <- tcExpr e
    case getExprDec et of
      TClass c -> do
        t <- tcField c i
        pure $ EProj a t et i
      t -> throwError $ notAClass t
  EMApp a () e i as -> do
    et <- tcExpr e
    case getExprDec et of
      TClass c -> do
        (rt, argst) <- tcMethod c i
        typedArgs <- mapM tcExpr as
        when (length as /= length argst) $
          throwError $ argNumMismatch (length argst) (length as)
        forM_ (zip argst typedArgs) $ \(expected, typed) ->
          assertType expected typed
        pure $ EMApp a rt et i typedArgs
      t -> throwError $ notAClass t
  ENew a () c i as -> do
    argst <- tcConstructor c i
    typedArgs <- mapM tcExpr as
    forM_ (zip argst typedArgs) $ \(expected, typed) ->
      assertType expected typed
    pure $ ENew a (TClass c) c i typedArgs


currentRetType :: Typechecker Type
currentRetType = maybe (error "fun env not in a funtion") id <$>
  view retType

newScope :: Typechecker a -> Typechecker a
newScope = local (set currentScopeVars S.empty)

tcStmt :: Stmt 'Untyped -> Typechecker (Stmt 'Typed)
tcStmt = \case
  SDecl a t v k -> do
    vars <- view currentScopeVars
    when (S.member v vars) $
      throwError $ duplicateVar v
    local (over definedVars (M.insert v t) . over currentScopeVars (S.insert v)) $
      SDecl a t v <$> tcStmt k
  SAssg a v e k -> do
    et <- tcExpr e
    vt <- tcVar v
    assertType vt et
    SAssg a v et <$> tcStmt k
  SFieldAssg a b f e k -> do
    bt <- tcExpr b
    et <- tcExpr e
    case getExprDec bt of
      TClass c -> do
        t <- tcField c f
        assertType t et
        SFieldAssg a bt f et <$> tcStmt k
      t -> throwError $ notAClass t
  SIncr a v k -> do
    vt <- tcVar v
    matchTypes TInt vt
    SIncr a v <$> tcStmt k
  SDecr a v k -> do
    vt <- tcVar v
    matchTypes TInt vt
    SDecr a v <$> tcStmt k
  SCond a c t k -> do
    ct <- tcExpr c
    assertType TBool ct
    tt <- newScope $ tcStmt t
    SCond a ct tt <$> tcStmt k
  SCondElse a c t e k -> do
    ct <- tcExpr c
    assertType TBool ct
    tt <- newScope $ tcStmt t
    et <- newScope $ tcStmt e
    SCondElse a ct tt et <$> tcStmt k
  SWhile a c b k -> do
    ct <- tcExpr c
    assertType TBool ct
    bt <- newScope $ tcStmt b
    SWhile a ct bt <$> tcStmt k
  SExp a e k -> do
    et <- tcExpr e
    SExp a et <$> tcStmt k
  SRet a e k -> do
    et <- tcExpr e
    rt <- currentRetType
    assertType rt et
    SRet a et <$> tcStmt k
  SVRet a k -> do
    rt <- currentRetType
    matchTypes rt TVoid
    SVRet a <$> tcStmt k
  SBlock a b k ->
    SBlock a <$> (newScope $ tcStmt b) <*> tcStmt k
  SEmpty a -> pure $ SEmpty a


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
  SExp _a (EApp _ _ (Id "error") _) _k -> True
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
                        , let t = f^.ty
                        , (i, _) <- NE.toList (f^.assignments)
                        ]
  , _ceMethods = M.fromList
                 [(i, (rt, as)) | (CMMethod m) <- c^.body
                          , let as = fmap (^.ty) $ m^.args
                                rt = m^.retType
                                i = m^.name
                          ]
  , _ceConstructors = M.fromList
    [(i, as) | (CMConstructor co) <- c^.body
             , let as = fmap (^.ty) $ co^.args
                   i = co^.name
             ]
  }

buildInitialEnv :: [TopDef 'Untyped] -> Except String TypecheckerEnv
buildInitialEnv defs = do
  when (null [() | TDFun fdef <- defs, fdef^.name == "main"]) $
    throwError noMain
  foldM (\prev d ->
           case d of
             TDFun fdef -> do
               when (fdef^.name == "main" && (fdef^.retType /= TInt || not (null $ fdef^.args))) $
                 throwError mainType
               when (fdef^.name `elem` (M.keys $ prev^.definedFuns)) $
                 throwError $ duplicateFun (fdef^.name)
               pure $
                 over definedFuns (M.insert (fdef^.name) (fdef^.retType, fmap (^.ty) (fdef^.args))) prev
             TDClass cd -> do
               when (cd^.name `elem` (M.keys $ prev^.classEnv)) $
                 throwError $ duplicateClass (cd^.name)
               let ce = buildClassEntry cd
                   checkDups :: (MonadError String m, Ord s)
                             => [x] -> (x -> s) -> (s -> String) -> m ()
                   checkDups vals getname err = flip evalStateT S.empty $ forM_ vals $ \f -> do
                     jeb <- gets $ S.member (getname f)
                     when jeb $ throwError $ err (getname f)
                     modify $ S.insert (getname f)
               checkDups [f | CMField fd <- cd^.body, (f, _) <- NE.toList $ fd^.assignments ] id duplicateField
               checkDups [m | CMMethod m <- cd^.body ] (^.name) duplicateMethod
               checkDups [c | CMConstructor c <- cd^.body ] (^.name) duplicateConstructor
               pure $ over classEnv (M.insert (cd^.name) ce) prev
        ) initialEnv defs


tcTopDef :: TopDef 'Untyped -> Typechecker (TopDef 'Typed)
tcTopDef = \case
  TDFun fdef -> do
    let addArgEnv =
          flip M.union (M.fromList $ fmap (\a -> (a^.name, a^.ty)) (fdef^.args))
    when (not $ (fdef^.retType == TVoid) || isReturning (fdef^.body)) $ throwError noReturn
    let bodyEnv = (over definedVars addArgEnv . set retType (Just $ fdef^.retType))
    tbody <- local bodyEnv (tcStmt $ fdef^.body)
    pure $ TDFun $ FunDef (fdef^.ann) (fdef^.retType) (fdef^.name) (fdef^.args) tbody
  TDClass cdef -> do
    let tcMember :: ClassMember 'Untyped -> Typechecker (ClassMember 'Typed)
        tcMember = \case
          CMMethod mdef -> do
            let addArgEnv =
                  flip M.union (M.fromList $ fmap (\a -> (a^.name, a^.ty)) (mdef^.args))
            when (maybe False (\b -> not $ (mdef^.retType == TVoid) || isReturning b) (mdef^.body)) $ throwError noReturn
            let setBodyEnv = over definedVars addArgEnv .
                             set retType (Just $ mdef^.retType) .
                             set currentFun (Just $ mdef^.name) .
                             set currentClass (Just $ cdef^.name)
            tbody <- case mdef^.body of
              Nothing -> pure Nothing
              Just b -> Just <$> local setBodyEnv (tcStmt b)
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
            let addArgEnv =
                  flip M.union (M.fromList $ fmap (\a -> (a^.name, a^.ty)) (codef^.args))
            when (not $ isReturning (codef^.body)) $ throwError noReturn
            let setBodyEnv = over definedVars addArgEnv .
                             set retType (Just $ TClass (cdef^.name)) .
                             set currentFun (codef^.name <|> Just (Id "unnamed constructor")) .
                             set currentClass (Just $ cdef^.name)
            tbody <- local setBodyEnv (tcStmt (codef^.body))
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

typecheck :: Program 'Untyped -> Either String (ClassEnv, Program 'Typed)
typecheck p@(Program defs) = runExcept $ do
  env <- buildInitialEnv defs
  runReaderT ((,) (env^.classEnv) <$> tcProgram p) env
