{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Latte.Frontend.IR.Compile(compile) where

import Latte.Frontend.IR.Types
import qualified Latte.Frontend.AST as AST
import qualified Latte.Frontend.Typechecker as Tc

import Data.Maybe(catMaybes)
import Data.String
import Data.List(foldl', nub)
import Data.Functor
import qualified Data.Map as M
import Control.Monad.RWS hiding ((<>))
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens hiding (Const)
import Prelude hiding ((<>))


makeConstructorName :: Int -> Maybe AST.ConstructorId -> String
makeConstructorName cid = \case
  Nothing -> "__cstr_" ++ show cid ++ "_unnamed_"
  Just n -> "__cstr_" ++ show cid ++ "_" ++ n^.AST.idStr


makeMethodName :: Int -> AST.MethodId -> String
makeMethodName ci mth = "__mth_" ++ show ci ++ "_" ++ mth^.AST.idStr


newSup :: Compiler Int
newSup = do
  s <- use supply
  modify (over supply (+1))
  return s


newVar :: Type -> Compiler VarId
newVar t = do
  i <- VarId <$> newSup
  modify $ over typeMap (M.insert i t)
  return i


registerVar :: Type -> AST.VarId -> Compiler VarId
registerVar t v = do
  i <- newVar t
  modify (over varMap (M.insert v i))
  return i


loadVar :: AST.VarId -> Compiler VarId
loadVar i = uses varMap (maybe (error $ "no var? " ++ i^.AST.idStr) id . M.lookup i)


localScope :: Compiler a -> Compiler a
localScope k = do
  s <- get
  r <- local (set defaultRet Nothing) k
  modify (set varMap (s^.varMap) . set typeMap (s^.typeMap))
  return r


makeLabel :: String -> Compiler Label
makeLabel s = do
  i <- newSup
  r <- view routineLabel
  return $ Label $ (lId r) ++ "_" ++ s ++ "_" ++ show i


write :: [Instr] -> Compiler ()
write is = modify $ over currentBlock (++is)


cLit :: AST.Lit -> Compiler Const
cLit = \case
  AST.LInt i -> return $ CInt i
  AST.LBool b -> return $ CInt (if b then 1 else 0)
  AST.LString s -> return $ CStr s


cType :: AST.Type -> TopCompiler Type
cType t = case t of
  AST.TInt -> pure (TInt 32)
  AST.TBool -> pure (TInt 1)
  AST.TString -> pure TString
  AST.TVoid -> pure TVoid
  AST.TClass c -> do
    cenv <- view classEnv
    i <- views classIds (M.! c)
    let processFields cc =
          let ce = cenv M.! cc in case (ce ^. AST.super) of
            Nothing -> mapM cType (map snd $ M.elems $ ce^.Tc.fields)
            Just x -> do
              sfs <- processFields x
              fs <- mapM cType (map snd $ M.elems $ ce^.Tc.fields)
              return (sfs ++ fs)
    fs <- processFields c
    return $ TObj i fs


mapConts :: (a -> (b -> Compiler d) -> Compiler d) -> [a] -> ([b] -> Compiler d) -> Compiler d
mapConts f l k =
  let build acc (h:t) = f h (\x -> build (x:acc) t)
      build acc [] = k (reverse acc)
  in build [] l


cExpr :: AST.Expr 'AST.Typed -> (Const -> Compiler a) -> Compiler a
cExpr ex k = case ex of
  AST.ELit _ _ l -> cLit l >>= k
  AST.EVar _ _ v -> CVar <$> loadVar v >>= k
  AST.EOp _ AST.TBool (AST.Op (AST.And _)) l r -> do
    tryR <- makeLabel "bool_and_right"
    setF <- makeLabel "bool_and_f"
    cont <- makeLabel "bool_and_cont"
    t <- liftTop $ cType AST.TBool
    v <- newVar t
    cExpr l $ \lv -> do
      cutBlock (Br (CondConst lv) tryR setF)
      newBlock tryR $ do
        cExpr r $ \rv -> do
          write [Assg t v (Const rv)]
          cutBlock (Jmp cont)
      newBlock setF $ do
        write [Assg t v (Const $ CInt 0)]
        cutBlock (Jmp cont)
      newBlock cont $ k (CVar v)
  AST.EOp _ AST.TBool (AST.Op (AST.Or _)) l r -> do
    tryR <- makeLabel "bool_or_right"
    setT <- makeLabel "bool_or_t"
    cont <- makeLabel "bool_or_cont"
    t <- liftTop $ cType AST.TBool
    v <- newVar t
    cExpr l $ \lv -> do
      cutBlock (Br (CondConst lv) setT tryR)
      newBlock tryR $ do
        cExpr r $ \rv -> do
          write [Assg t v (Const rv)]
          cutBlock (Jmp cont)
      newBlock setT $ do
        write [Assg t v (Const $ CInt 1)]
        cutBlock (Jmp cont)
      newBlock cont $ k (CVar v)
  AST.EOp _ te o l r -> cExpr l $ \lv -> cExpr r $ \rv -> do
    t <- liftTop $ cType te
    v <- newVar t
    write $ case o of
      AST.Op (AST.Plus _)  -> case te of
        AST.TString -> [Assg t v (Call "stringConcat" [lv, rv])]
        _ -> [Assg t v (NumOp Add lv rv)]
      AST.Op (AST.Minus _) -> [Assg t v (NumOp Sub lv rv)]
      AST.Op (AST.Mult _)  -> [Assg t v (NumOp Mul lv rv)]
      AST.Op (AST.Div _)   -> [Assg t v (NumOp Div lv rv)]
      AST.Op (AST.Mod _)   -> [Assg t v (NumOp Mod lv rv)]
      AST.Op (AST.And _)   -> [Assg t v (NumOp And lv rv)]
      AST.Op (AST.Or _)    -> [Assg t v (NumOp Or lv rv)]
      AST.Op (AST.LT _)    -> [Assg t v (RelOp Lt lv rv)]
      AST.Op (AST.LEQ _)   -> [Assg t v (RelOp Le lv rv)]
      AST.Op (AST.EQ _)    -> [Assg t v (RelOp Eq lv rv)]
      AST.Op (AST.NEQ _)   -> [Assg t v (RelOp Neq lv rv)]
      AST.Op (AST.GEQ _)   -> [Assg t v (RelOp Ge lv rv)]
      AST.Op (AST.GT _)    -> [Assg t v (RelOp Gt lv rv)]
    k $ CVar v
  AST.EUnOp _ te o e -> cExpr e $ \ee -> do
    let uo = case o of
          AST.Not -> Not
          AST.Neg -> Neg
    t <- liftTop $ cType te
    v <- newVar t
    write [Assg t v (UnOp uo ee)]
    k $ CVar v
  AST.EApp _ rt f as -> do
    tailRec <- view tailCall
    local (set tailCall False) $ (mapConts cExpr as) $ \asRefs -> do
      t <- liftTop $ cType rt
      v <- newVar t
      if tailRec
        then do
        cutBlock (TailCall (f^.AST.idStr) asRefs)
        k $ CVar v
        else do
        write $ [Assg t v (Call (f^.AST.idStr) asRefs)]
        k $ CVar v
  AST.EProj _ t e fld -> cExpr e $ \ce -> do
    tt <- liftTop $ cType t
    v <- newVar tt
    let (AST.TClass c) = AST.getExprDec e
    offset <- liftTop $ views fieldEnv (M.! (c,fld))
    write [Assg tt v (Proj ce offset)]
    k $ CVar v
  AST.EMApp _ rt e m as -> do
    _tailRec <- view tailCall
    local (set tailCall False) $ cExpr e $ \ce -> mapConts cExpr as $ \asRefs -> do
      t <- liftTop $ cType rt
      v <- newVar t
      write $ [Assg t v (VCall (m^.AST.idStr) (ce:asRefs))]
      k $ CVar v
  AST.ENew _ t c n as -> mapConts cExpr as $ \asRefs -> do
    tt <- liftTop $ cType t
    alloc <- newVar tt
    construct <- newVar tt
    cid <- liftTop $ views classIds (M.! c)
    let labelName = makeConstructorName cid n
    write [ Assg tt alloc NewObj
          , Assg tt construct (Call labelName (CVar alloc:asRefs))
          ]
    k $ CVar construct


cCondJump :: AST.Expr 'AST.Typed -> Label -> Label -> Compiler ()
cCondJump e ltrue lfalse =
  let relCond o l r = cExpr l $ \lv -> cExpr r $ \rv -> do
        cutBlock (Br (Cond o lv rv) ltrue lfalse)
      naiveCond = cExpr e $ \ve -> do
        cutBlock (Br (CondConst ve) ltrue lfalse)
  in case e of
    AST.EOp _ _ o l r -> case o of
      AST.Op (AST.Or _)   -> do
        step <- makeLabel "cond_step_or"
        cCondJump l ltrue step
        newBlock step $
          cCondJump r ltrue lfalse
      AST.Op (AST.And _)   -> do
        step <- makeLabel "cond_step_and"
        cCondJump l step lfalse
        newBlock step $
          cCondJump r ltrue lfalse
      AST.Op (AST.LT _)    -> relCond Lt l r
      AST.Op (AST.LEQ _)   -> relCond Le l r
      AST.Op (AST.EQ _)    -> relCond Eq l r
      AST.Op (AST.NEQ _)   -> relCond Neq l r
      AST.Op (AST.GEQ _)   -> relCond Ge l r
      AST.Op (AST.GT _)    -> relCond Gt l r
      _ -> naiveCond
    _ -> naiveCond


cutBlock :: FinInstr -> Compiler ()
cutBlock f = do
  bname <- view blockName
  b <- use currentBlock
  tell [Block bname b f]
  modify (set currentBlock [])


newBlock :: Label -> Compiler a -> Compiler a
newBlock i = local (set blockName i)


newBlockCont :: Label -> Label -> Compiler a -> Compiler a
newBlockCont i c = local (set nextBlock $ Just c) . newBlock i


cStmt :: AST.Stmt 'AST.Typed -> Compiler ()
cStmt = \case
  AST.SAssg _ v e k -> cExpr e $ \ve -> do
    t <- liftTop $ cType (AST.getExprDec e)
    vv <- loadVar v
    write [Assg t vv (Const ve)]
    cStmt k
  AST.SFieldAssg _ b f e k -> cExpr b $ \vb -> cExpr e $ \ve -> do
    t <- liftTop $ cType (AST.getExprDec e)
    let (AST.TClass c) = AST.getExprDec b
    offset <- liftTop $ views fieldEnv (M.! (c, f))
    write [FieldAssg t vb offset (Const ve)]
    cStmt k
  AST.SDecl _ t v k -> do
    tt <- liftTop $ cType t
    vi <- registerVar tt v
    case tt of
      TObj _ _ -> write [Assg tt vi NewObj]
      TInt _ -> write [Assg tt vi (Const $ CInt 0)]
      TString -> write [Assg tt vi (Const $ CStr "")]
      TVoid -> pure ()
    cStmt k
  AST.SIncr _ v k -> do
    t <- liftTop $ cType AST.TInt
    vv <- loadVar v
    write [Assg t vv (NumOp Add (CVar vv) (CInt 1))]
    cStmt k
  AST.SDecr _ v k -> do
    t <- liftTop $ cType AST.TInt
    vv <- loadVar v
    write [Assg t vv (NumOp Sub (CVar vv) (CInt 1))]
    cStmt k
  AST.SVRet _ _ -> do
    cutBlock (Ret Nothing)
  AST.SRet _ e@(AST.EApp _ _ f _) _ -> do
    myF <- views currentRoutine (fmap AST.FunId)
    if (myF == Just f)
      then local (set tailCall True) $ cExpr e $ \_ -> return ()
      else cExpr e $ \ve -> cutBlock (Ret (Just ve))
  AST.SRet _ e _ -> cExpr e $ \ve ->
    cutBlock (Ret (Just ve))
  AST.SCond _ e b k -> do
    cl <- makeLabel "if_cont"
    bl <- makeLabel "if_body"
    cCondJump e bl cl
    newBlockCont bl cl (localScope $ cStmt b)
    newBlock cl (cStmt k)
  AST.SCondElse _ e tb eb k -> do
    cl <- makeLabel "if_cont"
    tl <- makeLabel "if_body_then"
    el <- makeLabel "if_body_else"
    cCondJump e tl el
    newBlockCont tl cl (localScope $ cStmt tb)
    newBlockCont el cl (localScope $ cStmt eb)
    newBlock cl (cStmt k)
  AST.SWhile _ e b k -> do
    cdl <- makeLabel "while_cond"
    bl <- makeLabel "while_body"
    cl <- makeLabel "while_cont"
    cutBlock (Jmp cdl)
    newBlock cdl $ do
      cCondJump e bl cl
    newBlockCont bl cdl (localScope $ cStmt b)
    newBlock cl (cStmt k)
  AST.SExp _ e@(AST.EApp _ _ "error" _) _ ->
    cExpr e $ \_ -> cutBlock Unreachable
  AST.SExp _ e k -> do
    cExpr e $ \_ -> cStmt k
  AST.SBlock _ b k -> do
    bl <- makeLabel "block"
    cl <- makeLabel "block"
    cutBlock (Jmp bl)
    newBlockCont bl cl (localScope $ cStmt b)
    newBlock cl (cStmt k)
  AST.SEmpty _ -> do
    n <- view nextBlock
    dret <- view defaultRet
    cutBlock $ case n of
      Just nb -> Jmp nb
      Nothing -> case dret of
        Nothing -> Unreachable
        Just r  -> Ret r


compileBody :: Maybe (Maybe Const) -> String -> St -> AST.Stmt 'AST.Typed
            -> TopCompiler [Block]
compileBody defRet rname st b =
  snd <$> liftCompiler (Label rname)
  (local (set defaultRet defRet . set currentRoutine (Just rname))
    (cStmt b)) st


cFunDef :: AST.FunDef 'AST.Typed -> TopCompiler Routine
cFunDef f = do
  argTypes <- mapM cType (f^.AST.args <&> (^.AST.ty))
  let argIds = map (VarId . negate) [1..length $ f^.AST.args]
      initVarMap = M.fromList $ zip (f^.AST.args <&> (^.AST.name)) argIds
      initTypeMap = M.fromList $ zip argIds argTypes
  body <- let retDeflt = if f^.AST.retType == AST.TVoid
                       then Just Nothing
                       else if f^.AST.name == "main"
                            then Just (Just (CInt 0))
                            else Nothing
          in compileBody retDeflt (f^.AST.name.AST.idStr)
             (initSt initVarMap initTypeMap) (f^.AST.body)
  return $ Routine False (Label $ f^.AST.name.AST.idStr) argIds body


cTopDef :: AST.TopDef 'AST.Typed -> TopCompiler [Routine]
cTopDef = \case
  AST.TDFun f -> pure <$> cFunDef f
  AST.TDClass c ->
    fmap concat $ forM (c^.AST.body) $ \case
    AST.CMMethod mth -> cMethod (c^.AST.name) mth
    AST.CMConstructor ctr -> cConstructor (c^.AST.name) ctr
    AST.CMField _ -> return []


cMethod :: AST.ClassId -> AST.Method 'AST.Typed -> TopCompiler [Routine]
cMethod cl mth@AST.Method{AST._methodBody = Just methodBody} = do
  let argsWithThis = AST.Arg { AST._argAnn = AST.fakeAnn
                             , AST._argName = "this"
                             , AST._argTy = AST.TClass cl
                             }
                     : mth^.AST.args
  argTypes <- mapM cType (argsWithThis <&> (^.AST.ty))
  classId <- views classIds (M.! cl)
  let argIds = map (VarId . negate) [1..length argsWithThis]
      initVarMap = M.fromList $ zip (argsWithThis <&> (^.AST.name)) argIds
      initTypeMap = M.fromList $ zip argIds argTypes
      labelName = makeMethodName classId (mth^.AST.name)
  r <- compileBody (guard (mth^.AST.retType == AST.TVoid) >> Just Nothing) labelName
       (initSt initVarMap initTypeMap) methodBody
  return [Routine True (Label labelName) argIds r]
cMethod _ _ = return []


cConstructor :: AST.ClassId -> AST.Constructor 'AST.Typed -> TopCompiler [Routine]
cConstructor cl ctr = do
  let argsWithThis = AST.Arg { AST._argAnn = AST.fakeAnn
                             , AST._argName = "this"
                             , AST._argTy = AST.TClass cl
                             }
                     : ctr^.AST.args
  argTypes <- mapM cType (argsWithThis <&> (^.AST.ty))
  classId <- views classIds (M.! cl)
  let argIds = map (VarId . negate) [1..length argsWithThis]
      initVarMap = M.fromList $ zip (argsWithThis <&> (^.AST.name)) argIds
      initTypeMap = M.fromList $ zip argIds argTypes
      labelName = makeConstructorName classId (ctr^.AST.name)
  r <- compileBody (Just (Just $ CVar (initVarMap M.! "this"))) labelName
       (initSt initVarMap initTypeMap) (ctr^.AST.body)
  return [Routine False (Label labelName) argIds r]


buildMethodMap :: TopCompiler MethodMap
buildMethodMap = do
  cenv <- view classEnv
  let classes = M.toList cenv
      allMethods = nub [m | (_, c) <- classes, m <- M.keys (c ^. Tc.methods)]
  flip execStateT [] $ forM_ allMethods $ \m -> do
    impls <- forM classes $ \(cn, _) -> do
      let search :: AST.ClassId -> Maybe AST.ClassId
          search c =
            let entry :: Tc.ClassEntry
                entry = cenv M.! c
            in case M.lookup m (entry^.Tc.methods) of
              Nothing -> case entry^.AST.super of
                Nothing -> Nothing
                Just csup -> search csup
              Just _ -> Just c
      case search cn of
        Nothing -> return Nothing
        Just cown -> do
          iown <- views classIds (M.! cown)
          i <- views classIds (M.! cn)
          let implName = makeMethodName iown m
          return $ Just (implName, i)
    let resMap = foldl' folder M.empty (catMaybes impls) where
          folder prev (iname, cid) = case M.lookup iname prev of
            Nothing -> M.insert iname [cid] prev
            Just cids -> M.insert iname (cid:cids) prev
    modify ((m, resMap):)


cProgram :: AST.Program 'AST.Typed -> TopCompiler IR
cProgram (AST.Program ts) = IR . join <$> mapM cTopDef ts


compile :: Tc.ClassEnv -> AST.Program 'AST.Typed -> (IR, MethodMap)
compile ce p = runReader ((,) <$> cProgram p <*> buildMethodMap) (initTopEnv ce)
