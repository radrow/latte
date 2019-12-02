{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Latte.LLVM where

import Latte.Types.Latte
import Latte.Types.AST
import qualified Latte.Types.Syntax as S
import Latte.Error
import Latte.Tardis
import Latte.StdLib

import Control.Applicative
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Map(Map)
import qualified Data.Map as M
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Reader as R
import Control.Monad.Except
import Llvm hiding (SRet)
import FastString
import Unique
import Control.Lens hiding (op)


data LabelType = LABELIFCONT | LABELIFELSE | LABELIFTHEN | LABELWHILEBACK | LABELWHILECONT | LABELWHILEBODY
data RegType = REGEXPR | REGBUF | REGLOAD Id | REGARG Id | REGLOCAL Id | REGGLOBAL Id | REGSTRING

labelTstr :: LabelType -> String
labelTstr = \case
  LABELIFCONT -> "if_cont"
  LABELIFTHEN -> "if_then"
  LABELIFELSE -> "if_else"
  LABELWHILEBACK -> "while_back"
  LABELWHILEBODY -> "while_body"
  LABELWHILECONT -> "while_cont"


regTstr :: RegType -> String
regTstr = \case
  REGEXPR -> "exp"
  REGBUF -> "buffer"
  REGLOAD i -> "load_" ++ iName i
  REGARG i -> "arg_" ++ iName i
  REGLOCAL i -> "local_" ++ iName i
  REGGLOBAL i -> "global_" ++ iName i
  REGSTRING -> "str"


data Supply = Supply
  { _supReg :: Int
  , _supStr :: Int
  , _supLabel :: Int
  }
makeLenses ''Supply

newSupply :: Supply
newSupply = Supply 0 0 0


type VarEnv = Map Id LlvmVar


data LocalEnv = LocalEnv
  { _lenvFunRetType :: Type
  , _lenvFunName    :: Id
  , _lenvTailE      :: Bool
  }
makeLenses ''LocalEnv


data CompilerState = CompilerState
  { _csVarEnv  :: VarEnv
  , _csSupply  :: Supply
  , _csStrs    :: Map String LMString
  , _csStrings :: [LMGlobal]
  }
makeLenses ''CompilerState


type GlobalCompiler = StateT CompilerState (Except String)
type LocalCompiler = ReaderT LocalEnv GlobalCompiler


buildOp :: Type -> Op -> LlvmVar -> LlvmVar -> LocalCompiler ([LlvmStatement], LlvmVar)
buildOp t o v1 v2 = do
  outReg <- lift $ newTypedReg REGEXPR t
  case o of
    Op (S.LT _)    -> pure ([Assignment outReg (Compare LM_CMP_Slt v1 v2)], outReg)
    Op (S.LEQ _)   -> pure ([Assignment outReg (Compare LM_CMP_Sle v1 v2)], outReg)
    Op (S.EQ _)    -> pure ([Assignment outReg (Compare LM_CMP_Eq v1 v2)], outReg)
    Op (S.NEQ _)   -> pure ([Assignment outReg (Compare LM_CMP_Ne v1 v2)], outReg)
    Op (S.GEQ _)   -> pure ([Assignment outReg (Compare LM_CMP_Sge v1 v2)], outReg)
    Op (S.GT _)    -> pure ([Assignment outReg (Compare LM_CMP_Sgt v1 v2)], outReg)
    Op (S.Plus _)  -> case t of
      TInt -> pure ([Assignment outReg (LlvmOp LM_MO_Add v1 v2)], outReg)
      TString -> do
        s1 <-  lift $ newTypedReg REGEXPR TString
        s2 <-  lift $ newTypedReg REGEXPR TString
        size1 <- lift $ newTypedReg REGEXPR TInt
        size2 <- lift $ newTypedReg REGEXPR TInt
        sumSize <- lift $ newTypedReg REGEXPR TInt
        outSize <- lift $ newTypedReg REGEXPR TInt
        outReg' <- lift $ newTypedReg REGEXPR t
        outReg'' <- lift $ newTypedReg REGEXPR t
        pure
          ( [ Assignment size1 $
              Call StdCall (LMGlobalVar "strlen"
                             (LMFunction strlenDecl) External Nothing Nothing Constant
                           ) [v1] []
            , Assignment size2 $
              Call StdCall (LMGlobalVar "strlen"
                             (LMFunction strlenDecl) External Nothing Nothing Constant
                           ) [v2] []
            , Assignment sumSize $ LlvmOp LM_MO_Add size1 size2
            , Assignment outSize $ LlvmOp LM_MO_Add sumSize (LMLitVar $ LMIntLit 1 i32)
            , Assignment outReg $
                 Call StdCall (LMGlobalVar "calloc"
                               (LMFunction callocDecl) External Nothing Nothing Constant
                              ) [LMLitVar $ LMIntLit 1 i32, outSize] []
            , Assignment outReg' $
                 Call StdCall (LMGlobalVar "strcat"
                               (LMFunction strcatDecl) External Nothing Nothing Constant
                              ) [outReg, v1] []
            , Assignment outReg'' $
              Call StdCall (LMGlobalVar "strcat"
                             (LMFunction strcatDecl) External Nothing Nothing Constant
                           ) [outReg', v2] []
            ]
          , outReg''
          )
    Op (S.Minus _) -> pure ([Assignment outReg (LlvmOp LM_MO_Sub v1 v2)], outReg)
    Op (S.Mult _)  -> pure ([Assignment outReg (LlvmOp LM_MO_Mul v1 v2)], outReg)
    Op (S.Div _)   -> pure ([Assignment outReg (LlvmOp LM_MO_SDiv v1 v2)], outReg)
    Op (S.Mod _)   -> pure ([Assignment outReg (LlvmOp LM_MO_SRem v1 v2)], outReg)
    Op (S.Or _)    -> pure ([Assignment outReg (LlvmOp LM_MO_Or v1 v2)], outReg)
    Op (S.And _)   -> pure ([Assignment outReg (LlvmOp LM_MO_And v1 v2)], outReg)


buildType :: Type -> LlvmType
buildType = \case
  TInt -> i32
  TBool -> i1
  TVoid -> LMVoid
  TString -> LMPointer i8


addVar :: Id -> LlvmVar -> LocalCompiler ()
addVar i v =
  modify $ over csVarEnv (M.insert i v)


getVar :: Id -> GlobalCompiler LlvmVar
getVar i = gets ((M.lookup i) . (^.csVarEnv)) >>= \case
  Nothing -> throwError $ undefinedVar i
  Just ptrreg -> pure ptrreg


loadVar :: Type -> Id -> LocalCompiler ([LlvmStatement], LlvmVar)
loadVar t i = do
  ptrreg <- lift $ getVar i
  reg <- lift $ newTypedReg (REGLOAD i) t
  pure ([Assignment reg (Load ptrreg)], reg)

newReg :: RegType -> GlobalCompiler (LlvmType -> LlvmVar)
newReg rt = do
  sup <- gets $ (^. supReg) . (^. csSupply)
  modify (over csSupply (set supReg $ sup + 1))
  pure $ LMNLocalVar (fsLit (regTstr rt ++ "_" ++ show sup))



newTypedReg :: RegType -> Type -> GlobalCompiler LlvmVar
newTypedReg rt t = ($ buildType t) <$> newReg rt


newPtrReg :: RegType -> Type -> GlobalCompiler LlvmVar
newPtrReg rt t = ($ LMPointer (buildType t)) <$> newReg rt


newLabel :: LabelType -> GlobalCompiler LlvmBlockId
newLabel lt = do
  sup <- gets $ (^. supLabel) . (^. csSupply)
  modify (over csSupply (set supLabel $ sup + 1))
  pure $ getUnique $ fsLit $ labelTstr lt ++ show sup


newString :: String -> LocalCompiler LlvmVar
newString s = do
  sup <- gets $ (^. supStr) . (^. csSupply)
  modify (over csSupply (set supStr $ sup + 1))
  fname <- view lenvFunName
  let len = length s + 1
      glob = LMGlobal
        { getGlobalVar = LMGlobalVar (fsLit $ "str_" ++ iName fname ++ "_" ++ show sup)
                         (LMPointer (LMArray len i8))
                         Private
                         Nothing
                         (Just 1)
                         Constant
        , getGlobalValue = Just $ LMStaticStr
                           (fsLit s)
                           (LMArray len i8)
        }
  modify $ over csStrings (glob:)
  pure $ getGlobalVar glob


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
  prevVE <- gets (^. csVarEnv)
  res <- act
  modify $ over csVarEnv (const prevVE)
  return res


expr :: Expr Type -> LocalCompiler ([LlvmStatement], LlvmVar)
expr = \case
  ELit _ t lit -> case lit of
    LInt i -> pure ([], LMLitVar $ LMIntLit i (buildType t))
    LBool b -> pure ([], LMLitVar $ LMIntLit (if b then 1 else 0) (buildType t))
    LString s -> do
      strReg <- lift $ newTypedReg REGSTRING TString
      strVar <- newString s
      pure ( [Assignment strReg
              (GetElemPtr True strVar
               [ LMLitVar $ LMIntLit 0 i64
               , LMLitVar $ LMIntLit 0 i64
               ])]
           , strReg
           )
  EVar _ t i -> do
    (loadcode, reg) <- loadVar t i
    pure (loadcode, reg)
  EApp _ t f args -> do
    (argCode, argRegs, ats) <- foldM (\(pcode, pregs, argts) e -> do
                         (ecode, ereg) <- expr e
                         let argt = getExprDec e
                         pure (ecode ++ pcode, ereg : pregs, argt : argts)
                         ) ([], [], []) (reverse args)
    reg <- lift $ newTypedReg REGEXPR t
    tailrec <- view lenvTailE
    let funVar = LMGlobalVar
          (fsLit $ iName f)
          (LMFunction $ buildFunDecl f t ats)
          Internal
          Nothing
          Nothing
          Constant
    pure $
      (argCode ++
       [ (case t of TVoid -> Expr; _ -> Assignment reg)
         (Call (if tailrec then TailCall else StdCall) funVar argRegs [])]
      , reg)
  ENeg _ t ve -> do
    reg <- lift $ newTypedReg REGEXPR t
    (code, v) <- expr ve
    pure (code ++ [Assignment reg (LlvmOp LM_MO_Sub (LMLitVar $ LMIntLit 0 (buildType t)) v)], reg)
  ENot _ t ve -> do
    reg <- lift $ newTypedReg REGEXPR t
    (code, v) <- expr ve
    pure (code ++ [Assignment reg (LlvmOp LM_MO_Sub (LMLitVar $ LMIntLit 1 (buildType t)) v)], reg)
  EOp _ t o l r -> do
    (codel, ll) <- expr l
    (coder, rr) <- expr r
    (codeo, res) <- buildOp t o ll rr
    return $ (codel ++ coder ++ codeo, res)


-- genCode :: ([LlvmStatement], Env -> Env) -> Compiler ([LlvmStatement], Env)
-- genCode (code, cont) = ask >>= \e -> pure (code, cont e)

andThen :: (Monoid mon, Monad m) => m mon -> m mon -> m mon
andThen m1 m2 = liftA2 (<>) m1 m2

stmt :: Stmt (Expr Type) -> LocalCompiler [LlvmStatement]
stmt = \case
  SDecl _ t decls k -> do
    let vtype = buildType t
        declareVar :: [LlvmStatement] -> (Id, Maybe (Expr Type)) -> LocalCompiler [LlvmStatement]
        declareVar pcode (i, e) = do
          preg <- lift $ newPtrReg (REGLOCAL i) t
          addVar i preg
          exprCode <- forM e $ \def -> do
            (ce, ve) <- expr def
            pure (ce ++ [Store ve preg])
          pure $ pcode ++
            [Assignment preg (Alloca vtype 1)] ++ (maybe [] id exprCode)
    foldM declareVar [] (NE.toList decls) `andThen` stmt k
  SAssg _ n v k -> do
    reg <- lift $ getVar n
    (vcode, vv) <- expr v
    pure (vcode ++ [Store vv reg]) `andThen` stmt k
  SIncr _ v k -> do
    vptr <- lift $ getVar v
    (lcode, lreg) <- loadVar TInt v
    incred <- lift $ newTypedReg REGBUF TInt
    pure
      (lcode ++
       [Assignment incred (LlvmOp LM_MO_Add lreg (LMLitVar $ LMIntLit 1 (buildType TInt)))] ++
       [Store incred vptr]
      ) `andThen` stmt k
  SDecr _ v k -> do
    vptr <- lift $ getVar v
    (lcode, lreg) <- loadVar TInt v
    decred <- lift $ newTypedReg REGBUF TInt
    pure
      (lcode ++
       [Assignment decred (LlvmOp LM_MO_Sub lreg (LMLitVar $ LMIntLit 1 (buildType TInt)))] ++
       [Store decred vptr]
      ) `andThen` stmt k
  SCond _ c t k -> do
    thenLabel <- lift $ newLabel LABELIFTHEN
    contLabel <- lift $ newLabel LABELIFCONT
    (ccode, cvar) <- expr c
    tcode <- scoped $ stmt t
    pure ( ccode ++
           [BranchIf cvar (LMLocalVar thenLabel LMLabel) (LMLocalVar contLabel LMLabel)] ++
           [MkLabel thenLabel] ++ tcode ++ [Branch (LMLocalVar contLabel LMLabel)] ++
           [MkLabel contLabel]
         ) `andThen` stmt k
  SCondElse _ c t e k -> do
    thenLabel <- lift $ newLabel LABELIFTHEN
    elseLabel <- lift $ newLabel LABELIFELSE
    contLabel <- lift $ newLabel LABELIFCONT
    (ccode, cvar) <- expr c
    tcode <- scoped $ stmt t
    ecode <- scoped $ stmt e
    let (contJump, contBlock) = case k of
          SEmpty -> ([], [])
          _ -> ([Branch (LMLocalVar contLabel LMLabel)], [MkLabel contLabel])
    pure ( ccode ++
           [BranchIf cvar (LMLocalVar thenLabel LMLabel) (LMLocalVar elseLabel LMLabel)] ++
           [MkLabel thenLabel] ++ tcode ++ contJump ++
           [MkLabel elseLabel] ++ ecode ++ contJump ++
           contBlock
         ) `andThen` stmt k
  SWhile _ c b k -> do
    condLabel <- lift $ newLabel LABELWHILEBACK
    bodyLabel <- lift $ newLabel LABELWHILEBODY
    contLabel <- lift $ newLabel LABELWHILECONT
    (ccode, cvar) <- expr c
    bcode <- scoped $ stmt b
    pure ( [Branch (LMLocalVar condLabel LMLabel)] ++ [MkLabel condLabel] ++
           ccode ++
           [BranchIf cvar (LMLocalVar bodyLabel LMLabel) (LMLocalVar contLabel LMLabel)] ++
           [MkLabel bodyLabel] ++ bcode ++ [Branch (LMLocalVar condLabel LMLabel)] ++
           [MkLabel contLabel]
         ) `andThen` stmt k
  SExp _ e k -> do
    (code, _) <- expr e
    pure code `andThen` stmt k
  SRet _ e@(EApp _ _ fname _) -> do
    myF <- asks (^. lenvFunName)
    (code, v) <- R.local (set lenvTailE $ if myF == fname then True else False) (expr e)
    pure $ code ++ [Return $ Just v]
  SRet _ e -> do
    (code, v) <- expr e
    pure $ code ++ [Return $ Just v]
  SVRet _ ->
    pure [Return Nothing]
  SBlock _ b k -> scoped (stmt b) `andThen` stmt k
  SEmpty -> pure []


topDef :: TopDef Type -> GlobalCompiler [LlvmFunction]
topDef = \case
  FunDef _ retType fname args body -> do
    argRegs <- mapM (\(Arg _ t n) -> pure (n, t, LMNLocalVar (fsLit $ iName n) (buildType t))) args
    argPtrRegs <- mapM (\(Arg _ t n) -> (,) n <$> newPtrReg (REGARG n) t) args
    let argAssgCode = zip argRegs argPtrRegs >>= \((_, tp, areg), (_, preg)) ->
          [ Assignment preg (Alloca (buildType tp) 1)
          , Store areg preg
          ]
        withArgs :: VarEnv -> VarEnv
        withArgs = M.union $ M.fromList argPtrRegs
    bodyCode <- runReaderT
      (scoped $ modify (over csVarEnv withArgs) >> stmt body) (LocalEnv retType fname False)
    let decl = buildFunDecl fname retType (fmap (\(Arg _ t _) -> t) args)
        def = LlvmFunction
              decl
              (fmap (\(Arg _ _ n) -> fsLit $ iName n) args)
              []
              Nothing
              Nothing
              [LlvmBlock (getUnique $ fsLit (iName fname))
                (argAssgCode ++ bodyCode)
              ]
    pure [def]



program :: Program Type -> Either String LlvmModule
program (Program defs) =
  let buildProgram = do
        funs <- concat <$> traverse topDef defs
        globs <- gets (^. csStrings)
        pure $ LlvmModule
             []
             []
             []
             (stdglobals ++ globs)
             stddecls
             (stddefs ++ funs)
      -- (varEnv, funEnv) = buildGlobalEnv defs
  in runExcept (evalStateT buildProgram
          (CompilerState M.empty
            (Supply 0 0 0)
            M.empty []
          ))
