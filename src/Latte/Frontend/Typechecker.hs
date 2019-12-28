{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Latte.Frontend.Typechecker where

import Latte.Frontend.AST
import Latte.Frontend.Error

import Data.Functor
import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.List.NonEmpty as NE
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens

import Prelude hiding (LT, GT, EQ)


type LangError = [String]

type Typechecker =
  ReaderT TypecheckerEnv (Except String)

type VarEnv   = Map Id Type
type FunEnv   = Map Id (Type, [Type])
type ClassEnv = Map Id ClassEntry
data ClassEntry = ClassEntry
  { _ceSuper :: Maybe Id
  , _ceFields :: VarEnv
  , _ceMethods :: FunEnv
  }


data TypecheckerEnv = TypecheckerEnv
  { _teDefinedVars :: VarEnv
  , _teDefinedFuns :: FunEnv
  , _teClassEnv    :: ClassEnv
  , _teLoc         :: Ann
  , _teRetType     :: Maybe Type
  }

emptyEnv :: TypecheckerEnv
emptyEnv = TypecheckerEnv
  { _teDefinedVars = M.empty
  , _teDefinedFuns = M.empty
  , _teClassEnv = M.empty
  , _teLoc = fakeAnn
  , _teRetType = Nothing
  }


makeLensesWith abbreviatedFields ''TypecheckerEnv
makeLensesWith abbreviatedFields ''ClassEntry


getClassEntry :: Id -> Typechecker ClassEntry
getClassEntry i = views classEnv (M.lookup i) >>= \case
  Nothing -> throwError $ undefinedClass i
  Just c -> pure c


tcVar :: Id -> Typechecker Type
tcVar v = (M.lookup v) <$> view definedVars >>= \case
  Nothing -> throwError $ undefinedVar v
  Just t -> pure t


tcFun :: Id -> Typechecker (Type, [Type])
tcFun v = (M.lookup v) <$> view definedFuns >>= \case
  Nothing -> throwError $ undefinedVar v
  Just t -> pure t


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
  EUnOp ann () o v -> do
    vt <- tcExpr v
    rt <- tcUnOp o (getExprDec vt)
    pure $ EUnOp ann rt o vt
  EOp ann () o l r -> do
    tl <- tcExpr l
    tr <- tcExpr r
    tres <- tcOp o (getExprDec tl) (getExprDec tr)
    pure $ EOp ann tres o tl tr


currentRetType :: Typechecker Type
currentRetType = maybe (error "fun env not in a funtion") id <$>
  view retType


tcStmt :: Stmt 'Untyped -> Typechecker (Stmt 'Typed)
tcStmt = \case
  SDecl ann t v k -> do
    local (over definedVars (M.insert v t)) $ SDecl ann t v <$> tcStmt k
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
    SWhile ann ct bt <$> tcStmt k
  SExp ann e k -> do
    et <- tcExpr e
    SExp ann et <$> tcStmt k
  SRet ann e k -> do
    et <- tcExpr e
    rt <- currentRetType
    assertType rt et
    SRet ann et <$> tcStmt k
  SVRet ann k -> do
    rt <- currentRetType
    matchTypes rt TVoid
    SVRet ann <$> tcStmt k
  SBlock ann b k ->
    SBlock ann <$> tcStmt b <*> tcStmt k
  SEmpty ann -> pure $ SEmpty ann


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
  SRet ann e _ -> True
  SVRet ann _ -> True
  SBlock ann b k ->
    isReturning b || isReturning k
  SEmpty _ -> False

buildClassEntry :: ClassDef 'Untyped -> ClassEntry
buildClassEntry c = ClassEntry
  { _ceSuper = c ^. super
  , _ceFields = M.fromList $
                [(i, t) | (CMField f) <- c^.body, let t = f^.ty, (i, _) <- NE.toList (f^.assignments)]
  }

buildInitialEnv :: [TopDef 'Untyped] -> Except String TypecheckerEnv
buildInitialEnv =
  foldM (\prev d ->
           case d of
             TDFun (FunDef _ rt fn args _) -> pure $
               over definedFuns (M.insert fn (rt, fmap (^.ty) args)) prev
             TDClass cd -> pure $ over classEnv (M.insert (cd^.name) (buildClassEntry cd)) prev
        ) emptyEnv


tcTopDef :: TopDef 'Untyped -> Typechecker (TopDef 'Typed)
tcTopDef = \case
  TDFun (FunDef ann rt fname args body) -> do
    let addArgEnv = flip M.union
          (M.fromList $ fmap (\(Arg _ t n) -> (n, t)) args)
    when (not $ isReturning body) $ throwError noReturn
    tbody <- local (over definedVars addArgEnv . set retType (Just rt)) (tcStmt body)
    pure $ TDFun $ FunDef ann rt fname args tbody


tcProgram :: Program 'Untyped -> Either String (Program 'Typed)
tcProgram (Program defs) = runExcept $ do
  env <- buildInitialEnv defs
  runReaderT (Program <$> mapM tcTopDef defs) env
