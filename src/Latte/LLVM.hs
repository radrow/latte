{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Latte.LLVM where

import Latte.Types.Syntax as S hiding (Op)
import Latte.Types.Free
import Latte.Types.Latte
import Latte.Error

import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Map(Map)
import qualified Data.Map as M
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Llvm
import FastString
import Unique
import Control.Lens hiding (op)


data Supply = Supply
  { _supReg :: Int
  , _supStr :: Int
  , _supLabel :: Int
  }
makeLenses ''Supply

newSupply :: Supply
newSupply = Supply 0 0 0


data Env = Env
  { _envVars  :: Map Id LlvmVar
  , _envTypes :: Map Id Type
  , _envFunRetType :: Type
  }
makeLenses ''Env

newEnv :: Env
newEnv = Env {_envTypes = M.empty, _envVars = M.empty}


data SupplyState = SupplyState
  { _stStrs   :: Map String LMString
  , _stSupply :: Supply
  }
makeLenses ''SupplyState


type Supplier = State SupplyState
type Compiler = ReaderT Env (Except String)


buildOp :: Op -> LlvmVar -> LlvmVar -> LlvmExpression
buildOp = \case
  Op (S.LT _)    -> Compare LM_CMP_Slt
  Op (S.LEQ _)   -> Compare LM_CMP_Sle
  Op (S.EQ _)    -> Compare LM_CMP_Eq
  Op (S.NEQ _)   -> Compare LM_CMP_Ne
  Op (S.GEQ _)   -> Compare LM_CMP_Sge
  Op (S.GT _)    -> Compare LM_CMP_Sgt
  Op (S.Plus _)  -> LlvmOp LM_MO_Add
  Op (S.Minus _) -> LlvmOp LM_MO_Sub
  Op (S.Mult _)  -> LlvmOp LM_MO_Mul
  Op (S.Div _)   -> LlvmOp LM_MO_SDiv
  Op (S.Mod _)   -> LlvmOp LM_MO_SRem
  Op (S.Or _)    -> LlvmOp LM_MO_Or
  Op (S.And _)   -> LlvmOp LM_MO_And


buildType :: Type -> LlvmType
buildType = \case
  TInt -> i32
  TBool -> i1
  TVoid -> LMVoid
  TString -> LMPointer i8
  TFun _ _ -> error "LLVM fun type"


addVar :: Id -> Type -> LlvmVar -> Env -> Env
addVar i t v = over envVars (M.insert i v) . over envTypes (M.insert i t)


getVar :: Id -> Compiler LlvmVar
getVar i = asks (M.lookup i . (^. envVars)) >>= \case
  Nothing -> throwError $ undefinedVar i
  Just ptrreg -> pure ptrreg


loadVar :: Id -> LlvmVar -> Compiler [LlvmStatement]
loadVar i reg = asks (M.lookup i . (^. envVars)) >>= \case
  Nothing -> throwError $ undefinedVar i
  Just ptrreg -> pure ([Assignment reg (Load ptrreg)])


typeOf :: Id -> Compiler Type
typeOf i = asks (M.lookup i . (^. envTypes)) >>= \case
  Nothing -> throwError $ undefinedVar i
  Just t -> pure t


newReg :: Supplier (LlvmType -> LlvmVar)
newReg = do
  sup <- (^. supReg) . (^. stSupply) <$> get
  modify (over stSupply (set supReg $ sup + 1))
  pure $ LMNLocalVar (fsLit ("ex_var" ++ show sup))


newTypedReg :: Type -> Supplier LlvmVar
newTypedReg t = ($ buildType t) <$> newReg


newPtrReg :: Type -> Supplier LlvmVar
newPtrReg t = ($ LMPointer (buildType t)) <$> newReg


newLabel :: Supplier LlvmBlockId
newLabel = do
  sup <- (^. supLabel) . (^. stSupply) <$> get
  modify (over stSupply (set supLabel $ sup + 1))
  pure $ mkBuiltinUnique sup


typecheckOp :: Op -> Type -> Type -> Compiler Type
typecheckOp o l r =
  let tc t = typeMatch t r >> typeMatch t l
  in case o of
    Op (S.LT _)    -> tc TInt $> TBool
    Op (S.LEQ _)   -> tc TInt $> TBool
    Op (S.EQ _)    -> typeMatch l r $> TBool
    Op (S.NEQ _)   -> typeMatch l r $> TBool
    Op (S.GEQ _)   -> tc TInt $> TBool
    Op (S.GT _)    -> tc TInt $> TBool
    Op (S.Plus _)  -> tc TInt $> TInt
    Op (S.Minus _) -> tc TInt $> TInt
    Op (S.Mult _)  -> tc TInt $> TInt
    Op (S.Div _)   -> tc TInt $> TInt
    Op (S.Mod _)   -> tc TInt $> TInt
    Op (S.Or _)    -> tc TBool $> TBool
    Op (S.And _)   -> tc TBool $> TBool


typecheckLit :: Lit -> Type
typecheckLit = \case
  LInt _ -> TInt
  LString _ -> TString
  LBool _ -> TBool

unptrType :: LlvmType -> LlvmType
unptrType = \case
  LMPointer t -> t
  _ -> error "Not a pointer!"

typeMatch :: Type -> Type -> Compiler ()
typeMatch a b = if a == b then pure () else throwError $ typeMatchError a b

typeAssert :: Type -> Id -> Compiler ()
typeAssert t v = typeOf v >>= typeMatch t

assertRetType :: Type -> Compiler ()
assertRetType t = asks (^. envFunRetType) >>= flip typeMatch t

type ExprProd = Compiler ([LlvmStatement], LlvmVar, Type)
type StmtProd = Compiler ([LlvmStatement], Env)
type TopProd = Except String [LlvmFunction]

expr :: ExprM ExprProd a -> Supplier a
expr = foldFree $ \case
  ELitF _ lit k -> case lit of
    LInt i -> pure $ k $ pure $ ([], LMLitVar $ LMIntLit i i32, TInt)
    LBool b -> pure $ k $ pure $ ([], LMLitVar $ LMIntLit (if b then 1 else 0) i1, TBool)
    LString _ -> error "Strings TODO"
  EVarF _ i k -> do
    reg <- newReg
    pure $ k $ do
      t <- typeOf i
      let tv = buildType t
      loadcode <- loadVar i (reg tv)
      pure (loadcode, (reg tv), t)
  -- EAppF _ f args k -> undefined
  ENegF _ ve k -> do
    reg <- newTypedReg TInt
    pure $ k $ do
      (code, v, t) <- ve
      typeMatch TInt t
      pure (code ++ [Assignment reg
              (LlvmOp LM_MO_Sub (LMLitVar $ LMIntLit 0 i32) v)], reg, TInt)
  ENotF _ ve k -> do
    reg <- ($ i32) <$> newReg
    pure $ k $ do
      (code, v, t) <- ve
      typeMatch TBool t
      pure (code ++ [Assignment reg
              (LlvmOp LM_MO_Sub (LMLitVar $ LMIntLit 1 i1) v)], reg, TBool)
  EOpF  _ o l r k -> do
    reg <- newReg
    pure . k $ do
      (codel, ll, lt) <- l
      (coder, rr, rt) <- r
      resT <- typecheckOp o lt rt
      let resreg = reg (buildType resT)
      return $ (codel ++ coder ++ [Assignment resreg (buildOp o ll rr)], resreg, resT)


genCode :: ([LlvmStatement], Env -> Env) -> Compiler ([LlvmStatement], Env)
genCode (code, cont) = ask >>= \e -> pure (code, cont e)


stmt :: StmtM ExprProd StmtProd a -> Supplier a
stmt = foldFree $ \case
  SDeclF _ t decls k -> do
    let vtype = buildType t
    vars <- mapM (const (newPtrReg t)) decls
    expComps <- mapM (\(i, me) -> case me of
                         Nothing -> pure (i, Nothing)
                         Just e -> pure (i, Just e)
                     ) decls
    pure $ k $ do
      e <- ask
      foldM ( \(pcode, penv) (r, (i, mdefExpr)) -> do
                evDefExpr <- traverse (\a -> a >>= \(_, _, vt) -> typeMatch t vt >> a) mdefExpr
                pure ( pcode ++
                       [Assignment r (Alloca vtype 1)] ++
                       case evDefExpr of
                         Nothing -> []
                         Just (ce, ve, _) -> ce ++ [Store ve r]
                     , addVar i t r penv
                     )
            ) ([], e) (zip (NE.toList vars) (NE.toList expComps))
  SAssgF _ n v k -> newReg >>= \reg -> pure $ k $ do
    (vcode, vv, vt) <- v
    typeAssert vt n
    lcode <- loadVar n (reg (buildType vt))
    genCode
      ( vcode ++ lcode ++ [Store vv (reg (buildType vt))]
      , id
      )
  SIncrF _ e k -> do
    preincred <- newTypedReg TInt
    incred <- newTypedReg TInt
    pure $ k $ do
      lcode <- loadVar e preincred
      ptr <- getVar e
      typeAssert TInt e
      genCode ( lcode ++
                [Assignment incred
                  (LlvmOp LM_MO_Add preincred (LMLitVar $ LMIntLit 1 i32))] ++
                [Store incred ptr]
              , id)
  SDecrF _ e k -> do
    predecred <- newTypedReg TInt
    decred <- newTypedReg TInt
    pure $ k $ do
      lcode <- loadVar e predecred
      ptr <- getVar e
      typeAssert TInt e
      genCode ( lcode ++
                [Assignment decred
                  (LlvmOp LM_MO_Sub predecred (LMLitVar $ LMIntLit 1 i32))] ++
                [Store decred ptr]
              , id)
  SCondF _ c t k -> do
    thenLabel <- newLabel
    contLabel <- newLabel
    pure $ k $ do
      (ccode, cvar, tc) <- c
      typeMatch TBool tc
      (tcode, _) <- t
      genCode ( ccode ++
                [BranchIf cvar (LMLocalVar thenLabel LMLabel) (LMLocalVar contLabel LMLabel)] ++
                [MkLabel thenLabel] ++ tcode ++ [Branch (LMLocalVar contLabel LMLabel)] ++
                [MkLabel contLabel]
              , id)
  SCondElseF _ c t e k -> do
    thenLabel <- newLabel
    elseLabel <- newLabel
    contLabel <- newLabel
    pure $ k $ do
      (ccode, cvar, tc) <- c
      typeMatch TBool tc
      (tcode, _) <- t
      (ecode, _) <- e
      genCode ( ccode ++
                [BranchIf cvar (LMLocalVar thenLabel LMLabel) (LMLocalVar elseLabel LMLabel)] ++
                [MkLabel thenLabel] ++ tcode ++ [Branch (LMLocalVar contLabel LMLabel)] ++
                [MkLabel elseLabel] ++ ecode ++ [Branch (LMLocalVar contLabel LMLabel)] ++
                [MkLabel contLabel]
              , id)
  SWhileF _ c b k -> do
    checkLabel <- newLabel
    bodyLabel <- newLabel
    contLabel <- newLabel
    pure $ k $ do
      (ccode, cvar, tc) <- c
      typeMatch TBool tc
      (bcode, _) <- b
      genCode ( [Branch (LMLocalVar checkLabel LMLabel)] ++ [MkLabel checkLabel] ++
                ccode ++
                [BranchIf cvar (LMLocalVar bodyLabel LMLabel) (LMLocalVar contLabel LMLabel)] ++
                [MkLabel bodyLabel] ++ bcode ++ [Branch (LMLocalVar checkLabel LMLabel)] ++
                [MkLabel contLabel]
              , id)
  SExpF _ e k -> pure $ k $ do
    (code, _, _) <- e
    genCode (code, id)
  SRetF _ e k -> pure $ k $ do
    (code, v, t) <- e
    assertRetType t
    genCode (code ++ [Return $ Just v], id)
  SVRetF _ k -> pure $ k $ do
    genCode ([Return Nothing], id)
  SBlockF _ stmts k -> pure $ k $ do
    e <- ask
    foldM (\(pcode, penv) st -> do
              (ncode, nenv) <- local (const penv) st
              pure (pcode ++ ncode, nenv)
          ) ([], e) stmts
  SEmptyF _ k -> pure $ k $ genCode ([], id)
  SLiftExprF e k -> k <$> expr e


topDef :: TopDefM ExprProd StmtProd TopProd a -> Supplier a
topDef = foldFree $ \case
  FunDefF _ retType fname args body k -> do
    argRegs <- mapM (\(Arg _ t n) -> newTypedReg t >>= \r -> pure (n, t, r)) args
    argPtrRegs <- mapM (\(Arg _ t n) -> (,) n <$> newPtrReg t) args
    pure $ k $ do
      let venv = M.fromList argPtrRegs
          tenv = M.fromList $ fmap (\(Arg _ t n) -> (n, t)) args
          argAssgCode = zip argRegs argPtrRegs >>= \((_, tp, reg), (_, arg)) ->
            [ Assignment reg (Alloca (buildType tp) 1)
            , Store reg arg
            ]
      (bodyCode, _) <- runReaderT body (Env venv tenv retType)
      let decl = LlvmFunctionDecl
                 (fsLit $ iName fname)
                 Internal
                 CC_Ccc
                 (buildType retType)
                 FixedArgs
                 (fmap (\(Arg _ t _) -> (buildType t, [])) args)
                 Nothing
          def = LlvmFunction
                decl
                (fmap (\(Arg _ _ n) -> fsLit $ iName n) args)
                []
                Nothing
                Nothing
                [LlvmBlock (mkCoVarUnique 2137)
                 (argAssgCode ++ bodyCode)
                ]
      pure [def]
  LiftStmtF s k -> k <$> stmt s
