{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Latte.LLVM where

import qualified Latte.Types.Syntax as S
import Latte.Types.Syntax(Stmt(..))
import Latte.Types.Latte
import Latte.Types.AST
import Latte.Error
import Latte.Tardis

import Control.Applicative
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Map(Map)
import qualified Data.Map as M
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Llvm hiding (SRet)
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


type FunEnv = Map Id (Type, [Type])
type VarEnv = Map Id (Type, LlvmVar)


data LocalEnv = LocalEnv
  { _lenvFunRetType :: Type
  , _lenvFunName :: Id
  }
makeLenses ''LocalEnv


data CompilerState = CompilerState
  { _csFunEnv :: FunEnv
  , _csVarEnv :: VarEnv
  , _csSupply :: Supply
  , _csStrs   :: Map String LMString
  }
makeLenses ''CompilerState


type GlobalCompiler = StateT CompilerState (Except String)
type LocalCompiler = ReaderT LocalEnv GlobalCompiler


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


addVar :: Id -> Type -> LlvmVar -> LocalCompiler ()
addVar i t v =
  modify $ over csVarEnv (M.insert i (t, v))


getVar :: Id -> GlobalCompiler (Type, LlvmVar)
getVar i = gets ((M.lookup i) . (^.csVarEnv)) >>= \case
  Nothing -> throwError $ undefinedVar i
  Just ptrreg -> pure ptrreg


typeOf :: Id -> GlobalCompiler Type
typeOf i = fst <$> getVar i


loadVar :: Id -> LocalCompiler ([LlvmStatement], LlvmVar)
loadVar i = do
  (t, ptrreg) <- lift $ getVar i
  reg <- lift $ newTypedReg t
  pure ([Assignment reg (Load ptrreg)], reg)


newReg :: GlobalCompiler (LlvmType -> LlvmVar)
newReg = do
  sup <- gets $ (^. supReg) . (^. csSupply)
  modify (over csSupply (set supReg $ sup + 1))
  pure $ LMNLocalVar (fsLit ("ex_var" ++ show sup))


newTypedReg :: Type -> GlobalCompiler LlvmVar
newTypedReg t = ($ buildType t) <$> newReg


newPtrReg :: Type -> GlobalCompiler LlvmVar
newPtrReg t = ($ LMPointer (buildType t)) <$> newReg


newLabel :: GlobalCompiler LlvmBlockId
newLabel = do
  sup <- gets $ (^. supLabel) . (^. csSupply)
  modify (over csSupply (set supLabel $ sup + 1))
  pure $ mkBuiltinUnique sup


typecheckOp :: Op -> Type -> Type -> GlobalCompiler Type
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

typeMatch :: Type -> Type -> GlobalCompiler ()
typeMatch a b = if a == b then pure () else throwError $ typeMatchError a b

typeAssert :: Type -> Id -> GlobalCompiler ()
typeAssert t v = typeOf v >>= typeMatch t


typeOfFun :: Id -> GlobalCompiler (Type, [Type])
typeOfFun i = gets (M.lookup i . (^.csFunEnv)) >>= \case
  Nothing -> throwError $ undefinedVar i
  Just t -> pure t


buildFunDecl :: Id -> Type -> [Type] -> LlvmFunctionDecl
buildFunDecl n t args = LlvmFunctionDecl
  (fsLit $ iName n)
  Internal
  CC_Ccc
  (buildType t)
  FixedArgs
  (fmap (\tt -> (buildType tt, [])) args)
  Nothing


scoped :: LocalCompiler a -> LocalCompiler a
scoped act = do
  prevFs <- get
  res <- act
  put prevFs
  return res


assertRetType :: Type -> LocalCompiler ()
assertRetType t = asks (^. lenvFunRetType) >>= \tf -> lift $ typeMatch tf t

expr :: Expr -> LocalCompiler ([LlvmStatement], LlvmVar, Type)
expr = \case
  ELit _ lit -> case lit of
    LInt i -> pure ([], LMLitVar $ LMIntLit i i32, TInt)
    LBool b -> pure ([], LMLitVar $ LMIntLit (if b then 1 else 0) i1, TBool)
    LString _ -> error "Strings TODO"
  EVar _ i -> do
    t <- lift $ typeOf i
    (loadcode, reg) <- loadVar i
    pure (loadcode, reg, t)
  EApp _ f args -> do
    (rt, ats) <- lift $ typeOfFun f
    (argCode, argRegs) <- foldM (\(pcode, pregs) (t, e) -> do
                         (ecode, ereg, et) <- expr e
                         lift $ typeMatch t et
                         pure (ecode ++ pcode, ereg : pregs)
                         ) ([], []) (reverse $ zip ats args)
    reg <- lift $ newTypedReg rt
    let funVar = LMGlobalVar
          (fsLit $ iName f)
          (LMFunction $ buildFunDecl f rt ats)
          Internal
          Nothing
          Nothing
          Constant
    pure $
      (argCode ++
       [Assignment reg
         (Call StdCall funVar argRegs [])]
      , reg, rt)
  ENeg _ ve -> do
    reg <- lift $ newTypedReg TInt
    (code, v, t) <- expr ve
    lift $ typeMatch TInt t
    pure (code ++ [Assignment reg
                   (LlvmOp LM_MO_Sub (LMLitVar $ LMIntLit 0 i32) v)], reg, TInt)
  ENot _ ve -> do
    reg <- lift $ newTypedReg TInt
    (code, v, t) <- expr ve
    lift $ typeMatch TBool t
    pure (code ++ [Assignment reg
                   (LlvmOp LM_MO_Sub (LMLitVar $ LMIntLit 1 i1) v)], reg, TBool)
  EOp _ o l r -> do
    reg <- lift newReg
    (codel, ll, lt) <- expr l
    (coder, rr, rt) <- expr r
    resT <- lift $ typecheckOp o lt rt
    let resreg = reg (buildType resT)
    return $ (codel ++ coder ++ [Assignment resreg (buildOp o ll rr)], resreg, resT)


-- genCode :: ([LlvmStatement], Env -> Env) -> Compiler ([LlvmStatement], Env)
-- genCode (code, cont) = ask >>= \e -> pure (code, cont e)


stmt :: S.Stmt Expr -> LocalCompiler [LlvmStatement]
stmt = \case
  SDecl _ t decls -> do
    let vtype = buildType t
        declareVar :: [LlvmStatement] -> (Id, Maybe Expr) -> LocalCompiler [LlvmStatement]
        declareVar pcode (i, e) = do
          preg <- lift $ newPtrReg t
          addVar i t preg
          exprCode <- forM e $ \def -> do
            (ce, ve, te) <- expr def
            lift $ typeMatch t te
            pure (ce ++ [Store ve preg])
          pure $ pcode ++
            [Assignment preg (Alloca vtype 1)] ++ (maybe [] id exprCode)
    foldM declareVar [] (NE.toList decls)
  SAssg _ n v -> do
    (rt, reg) <- lift $ getVar n
    (vcode, vv, vt) <- expr v
    lift $ typeMatch rt vt
    pure $ vcode ++ [Store vv reg]
  SIncr _ v -> do
    (t, vptr) <- lift $ getVar v
    (lcode, lreg) <- loadVar v
    lift $ typeMatch TInt t
    incred <- lift $ newTypedReg TInt
    pure $ lcode ++
      [Assignment incred
       (LlvmOp LM_MO_Add lreg (LMLitVar $ LMIntLit 1 i32))] ++
      [Store incred vptr]
  SDecr _ v -> do
    (t, vptr) <- lift $ getVar v
    (lcode, lreg) <- loadVar v
    lift $ typeMatch TInt t
    decred <- lift $ newTypedReg TInt
    pure $ lcode ++
      [Assignment decred
       (LlvmOp LM_MO_Sub lreg (LMLitVar $ LMIntLit 1 i32))] ++
      [Store decred vptr]
  SCond _ c t -> do
    thenLabel <- lift newLabel
    contLabel <- lift newLabel
    (ccode, cvar, tc) <- expr c
    lift $ typeMatch TBool tc
    tcode <- scoped $ stmt t
    pure ( ccode ++
           [BranchIf cvar (LMLocalVar thenLabel LMLabel) (LMLocalVar contLabel LMLabel)] ++
           [MkLabel thenLabel] ++ tcode ++ [Branch (LMLocalVar contLabel LMLabel)] ++
           [MkLabel contLabel]
         )
  SCondElse _ c t e -> do
    thenLabel <- lift newLabel
    elseLabel <- lift newLabel
    contLabel <- lift newLabel
    (ccode, cvar, tc) <- expr c
    lift $ typeMatch TBool tc
    tcode <- scoped $ stmt t
    ecode <- scoped $ stmt e
    pure ( ccode ++
           [BranchIf cvar (LMLocalVar thenLabel LMLabel) (LMLocalVar elseLabel LMLabel)] ++
           [MkLabel thenLabel] ++ tcode ++ [Branch (LMLocalVar contLabel LMLabel)] ++
           [MkLabel elseLabel] ++ ecode ++ [Branch (LMLocalVar contLabel LMLabel)] ++
           [MkLabel contLabel]
         )
  SWhile _ c b -> do
    checkLabel <- lift newLabel
    bodyLabel <- lift newLabel
    contLabel <- lift newLabel
    (ccode, cvar, tc) <- expr c
    lift $ typeMatch TBool tc
    bcode <- scoped $ stmt b
    pure ( [Branch (LMLocalVar checkLabel LMLabel)] ++ [MkLabel checkLabel] ++
           ccode ++
           [BranchIf cvar (LMLocalVar bodyLabel LMLabel) (LMLocalVar contLabel LMLabel)] ++
           [MkLabel bodyLabel] ++ bcode ++ [Branch (LMLocalVar checkLabel LMLabel)] ++
           [MkLabel contLabel]
         )
  SExp _ e -> do
    (code, _, _) <- expr e
    pure code
  SRet _ e -> do
    (code, v, t) <- expr e
    assertRetType t
    pure $ code ++ [Return $ Just v]
  SVRet _ ->
    pure [Return Nothing]
  SBlock _ stmts -> scoped $ do
    foldM (\pcode st -> do
              ncode <- stmt st
              pure (pcode ++ ncode)
          ) [] stmts
  SEmpty _ -> pure []


topDef :: TopDef -> GlobalCompiler [LlvmFunction]
topDef = \case
  FunDef _ retType fname args body -> do
    argRegs <- mapM (\(Arg _ t n) -> pure (n, t, LMNLocalVar (fsLit $ iName n) (buildType t))) args
    argPtrRegs <- mapM (\(Arg _ t n) -> (,) n <$> newPtrReg t) args
    let argAssgCode = zip argRegs argPtrRegs >>= \((_, tp, areg), (_, preg)) ->
          [ Assignment preg (Alloca (buildType tp) 1)
          , Store areg preg
          ]
        withArgs :: VarEnv -> VarEnv
        withArgs = M.union $ M.fromList $ fmap (\((n, r), (Arg _ t _)) -> (n, (t, r))) (zip argPtrRegs args)
    bodyCode <- runReaderT (scoped $ modify (over csVarEnv withArgs) >> stmt body) (LocalEnv retType fname)
    let decl = buildFunDecl fname retType (fmap (\(Arg _ t _) -> t) args)
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


buildGlobalEnv :: [TopDef] -> (VarEnv, FunEnv)
buildGlobalEnv = foldl (\(pv, pf) d -> case d of
                           FunDef _ rt fn args _ ->
                             (pv, M.insert fn (rt, fmap (\(Arg _ t _) -> t) args) pf)
                           ) (M.empty, M.empty)


program :: Program -> Either String LlvmModule
program (Program defs) =
  let buildProgram = do
        funs <- concat <$> traverse topDef defs
        pure $ LlvmModule
             []
             []
             []
             []
             []
             funs
      (varEnv, funEnv) = buildGlobalEnv defs
  in runExcept (evalStateT buildProgram
          (CompilerState funEnv varEnv
            (Supply 0 0 0)
            M.empty
          ))
