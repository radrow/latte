{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Latte.LLVM where

import Latte.Types.Syntax as S hiding (Op)
import Latte.Types.Free

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


op :: Op -> LlvmVar -> LlvmVar -> LlvmExpression
op = \case
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


type_ :: Type -> LlvmType
type_ = \case
  TInt _ -> i32
  TBool _ -> i1
  TVoid _ -> LMVoid
  TString _ -> LMPointer i8
  TFun _ _ _ -> error "LLVM fun type"


data Supply = Supply
  { _supReg :: Int
  , _supStr :: Int
  , _supLabel :: Int
  }
newSupply :: Supply
newSupply = Supply 0 0 0

makeLenses ''Supply

data Env = Env
  { _envVars  :: Map Id LlvmVar
  , _envTypes :: Map Id LlvmType
  }
newEnv = Env {_envVars = M.empty}
makeLenses ''Env

data SupplyState = SupplyState
  { _stStrs   :: Map String LMString
  , _stSupply :: Supply
  }
makeLenses ''SupplyState

withVar :: Id -> LlvmVar -> Env -> Env
withVar i v = over envVars $ M.insert i v

loadVar :: Id -> (LlvmType -> LlvmVar) -> Compiler ([LlvmStatement], LlvmVar, LlvmVar)
loadVar i r = asks (M.lookup i . (^. envVars)) >>= \case
  Nothing -> throwError $ "Undefined value: " <> iName i
  Just ptrreg ->
    let rt = r (unptrType $ typecheckVar ptrreg)
    in pure ([Assignment rt (Load ptrreg)], ptrreg, rt)

type Supplier = State SupplyState
type Compiler = ExceptT String (Reader Env)


regVar :: Supplier (LlvmType -> LlvmVar)
regVar = do
  sup <- (^. supReg) . (^. stSupply) <$> get
  modify (over stSupply (set supReg $ sup + 1))
  pure $ LMNLocalVar (fsLit ("ex_var" ++ show sup))

label :: Supplier LlvmBlockId
label = do
  sup <- (^. supLabel) . (^. stSupply) <$> get
  modify (over stSupply (set supLabel $ sup + 1))
  pure $ mkBuiltinUnique sup

typecheckOp :: Op -> LlvmType
typecheckOp = \case
  Op (S.LT _)    -> i1
  Op (S.LEQ _)   -> i1
  Op (S.EQ _)    -> i1
  Op (S.NEQ _)   -> i1
  Op (S.GEQ _)   -> i1
  Op (S.GT _)    -> i1
  Op (S.Plus _)  -> i32
  Op (S.Minus _) -> i32
  Op (S.Mult _)  -> i32
  Op (S.Div _)   -> i32
  Op (S.Mod _)   -> i32
  Op (S.Or _)    -> i1
  Op (S.And _)   -> i1
typecheckVar :: LlvmVar -> LlvmType
typecheckVar = \case
    LMLocalVar _ tp -> tp
    LMNLocalVar _ tp -> tp
    LMLitVar l -> typecheckLit l
    LMGlobalVar _ tp _ _ _ _ -> tp

typecheckLit :: LlvmLit -> LlvmType
typecheckLit = \case
  LMIntLit _ tp -> tp
  LMFloatLit _ tp -> tp
  LMUndefLit tp -> tp
  LMVectorLit ts -> LMStruct (map typecheckLit ts)
  LMNullLit tp -> tp

unptrType :: LlvmType -> LlvmType
unptrType = \case
  LMPointer t -> t
  _ -> error "Not a pointer!"

typeAssert :: LlvmType -> LlvmVar -> Compiler ()
typeAssert t v = if typecheckVar v == t then pure () else throwError $ "Type error"

expr :: ExprM (Compiler ([LlvmStatement], LlvmVar)) a -> Supplier a
expr = foldFree $ \case
  ELitF _ lit k -> case lit of
    LInt i -> pure $ k $ pure $ ([], LMLitVar $ LMIntLit i i32)
    LBool b -> pure $ k $ pure $ ([], LMLitVar $ LMIntLit (if b then 1 else 0) i1)
    LString _ -> error "Strings TODO"
  EVarF _ i k -> do
    reg <- regVar
    pure $ k $ do
      (loadcode, _, regt) <- loadVar i reg
      pure (loadcode, regt)
  -- EAppF _ f args k -> undefined
  ENegF _ ve k -> do
    reg <- ($ i32) <$> regVar
    pure $ k $ do
      (code, v) <- ve
      typeAssert i32 v
      pure (code ++ [Assignment reg
              (LlvmOp LM_MO_Sub (LMLitVar $ LMIntLit 0 i32) v)], reg)
  ENotF _ ve k -> do
    reg <- ($ i32) <$> regVar
    pure $ k $ do
      (code, v) <- ve
      typeAssert i1 v
      pure (code ++ [Assignment reg
              (LlvmOp LM_MO_Sub (LMLitVar $ LMIntLit 1 i1) v)], reg)
  EOpF  _ o l r k -> do
    reg <- ($ typecheckOp o) <$> regVar
    pure . k $ do
      (codel, ll) <- l
      (coder, rr) <- r
      return $ (codel ++ coder ++ [Assignment reg (op o ll rr)], reg)

genCode :: ([LlvmStatement], Env -> Env) -> Compiler ([LlvmStatement], Env)
genCode (code, cont) = ask >>= \e -> pure (code, cont e)

stmt :: StmtM (Compiler ([LlvmStatement], LlvmVar)) (Compiler ([LlvmStatement], Env)) a
     -> Supplier a
stmt = foldFree $ \case
  SDeclF _ t decls k -> do
    let vtype = type_ t
    vars <- mapM (const (($ LMPointer vtype) <$> regVar)) decls
    expComps <- mapM (\(i, me) -> case me of
                         Nothing -> pure (i, Nothing)
                         Just e -> pure (i, Just e)
                     ) decls
    pure $ k $ do
      e <- ask
      foldM ( \(pcode, penv) (r, (i, mdefExpr)) -> do
                evDefExpr <- traverse (\a -> a >>= \(_, ve) -> typeAssert vtype ve >> a) mdefExpr
                pure ( pcode ++
                       [Assignment r (Alloca vtype 1)] ++
                       case evDefExpr of
                         Nothing -> []
                         Just (ce, ve) -> ce ++ [Store ve r]
                     , withVar i r penv
                     )
            ) ([], e) (zip (NE.toList vars) (NE.toList expComps))
  SAssgF _ n v k -> regVar >>= \vr -> pure $ k $ do
    (vcode, vv) <- v
    (lcode, reg, prev) <- loadVar n vr
    typeAssert (typecheckVar prev) vv
    genCode
      ( vcode ++ lcode ++ [Store vv reg]
      , id
      )
  SIncrF _ e k -> do
    preincred <- regVar
    incred <- ($ i32) <$> regVar
    pure $ k $ do
      (lcode, ptr, r) <- loadVar e preincred
      typeAssert i32 r
      genCode ( lcode ++
                [Assignment incred (LlvmOp LM_MO_Add r (LMLitVar $ LMIntLit 1 i32))] ++
                [Store incred ptr]
              , id)
  SDecrF _ e k -> do
    predecred <- regVar
    decred <- ($ i32) <$> regVar
    pure $ k $ do
      (lcode, ptr, r) <- loadVar e predecred
      typeAssert i32 r
      genCode ( lcode ++
                [Assignment decred (LlvmOp LM_MO_Sub r (LMLitVar $ LMIntLit 1 i32))] ++
                [Store decred ptr]
              , id)
  SCondF _ c t k -> do
    thenLabel <- label
    contLabel <- label
    pure $ k $ do
      (ccode, cvar) <- c
      typeAssert i1 cvar
      (tcode, _) <- t
      genCode ( ccode ++
                [BranchIf cvar (LMLocalVar thenLabel LMLabel) (LMLocalVar contLabel LMLabel)] ++
                [MkLabel thenLabel] ++ tcode ++ [Branch (LMLocalVar contLabel LMLabel)] ++
                [MkLabel contLabel] ++ ccode
              , id)
  SCondElseF _ c t e k -> do
    thenLabel <- label
    elseLabel <- label
    contLabel <- label
    pure $ k $ do
      (ccode, cvar) <- c
      typeAssert i1 cvar
      (tcode, _) <- t
      (ecode, _) <- e
      genCode ( ccode ++
                [BranchIf cvar (LMLocalVar thenLabel LMLabel) (LMLocalVar elseLabel LMLabel)] ++
                [MkLabel thenLabel] ++ tcode ++ [Branch (LMLocalVar contLabel LMLabel)] ++
                [MkLabel elseLabel] ++ ecode ++ [Branch (LMLocalVar contLabel LMLabel)] ++
                [MkLabel contLabel] ++ ccode
              , id)
  SWhileF _ c b k -> do
    checkLabel <- label
    bodyLabel <- label
    contLabel <- label
    pure $ k $ do
      (ccode, cvar) <- c
      typeAssert i1 cvar
      (bcode, _) <- b
      genCode ( [Branch (LMLocalVar checkLabel LMLabel)] ++ [MkLabel checkLabel] ++
                ccode ++
                [BranchIf cvar (LMLocalVar bodyLabel LMLabel) (LMLocalVar contLabel LMLabel)] ++
                [MkLabel bodyLabel] ++ bcode ++ [Branch (LMLocalVar checkLabel LMLabel)] ++
                [MkLabel contLabel] ++ ccode
              , id)
  SExpF _ e k -> pure $ k $ do
    (code, _) <- e
    genCode (code, id)
  SRetF _ e k -> pure $ k $ do
    (code, v) <- e
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
