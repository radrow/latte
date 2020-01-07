{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
module Latte.Frontend.IR where

import qualified Latte.Frontend.AST as AST
import qualified Latte.Frontend.Typechecker as Tc

import Text.PrettyPrint.HughesPJClass
import Data.Maybe(catMaybes)
import Data.String
import Data.List(foldl', nub)
import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.RWS hiding ((<>))
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens hiding (Const)
import Prelude hiding ((<>))

data VarId = VarId {vId :: Int} deriving (Ord, Eq)
data Label = Label {lId :: String} deriving (Ord, Eq)

data Type = TInt Int | TString | TVoid | TObj Int [Type]

data IR = IR [Routine]

data Routine = Routine Label [VarId] [Block]

data Block = Block Label [Instr] FinInstr

data Instr
  = Assg Type VarId Expr
  | FieldAssg Type Const Int Expr

data FinInstr
  = Ret (Maybe Const)
  | Jmp Label
  | Br Cond Label Label
  | Unreachable

data Cond = Cond RelOp Const Const | CondConst Const

data Expr
  = NumOp NumOp Const Const
  | RelOp RelOp Const Const
  | UnOp UnOp Const
  | Const Const
  | Call String [Const] -- result fname args
  | VCall String [Const]
  | NewObj
  | Proj Const Int

data UnOp = Neg | Not
data NumOp = Add | Sub | Mul | Div | Mod | Or | And
data RelOp = Eq | Neq | Lt | Le | Gt | Ge

data Const = CVar VarId | CInt Integer | CStr String

type VarMap = M.Map AST.Id VarId
type TypeMap = M.Map VarId Type
type FieldMap = M.Map (AST.Id, AST.Id) Int
-- | [(method, implementation -> classes)]
type MethodMap = [(AST.Id, M.Map String [Int])]

data St = St
  { _stSupply :: Int
  , _stCurrentBlock :: [Instr]
  , _stVarMap :: VarMap
  , _stTypeMap :: TypeMap
  }
initSt :: VarMap -> TypeMap -> St
initSt vm tm = St 0 [] vm tm

data Env = Env
  { _envRoutineLabel :: Label
  , _envBlockName    :: Label
  , _envNextBlock    :: Maybe Label
  , _envTopEnv       :: TopEnv
  , _envCurrentClass :: Maybe AST.Id
  , _envVirtReturn   :: Bool
  }
initEnv :: Label -> Label -> TopCompiler Env
initEnv r l = ask >>= \te -> pure Env
  { _envRoutineLabel = r
  , _envBlockName = l
  , _envNextBlock = Nothing
  , _envTopEnv = te
  , _envCurrentClass = Nothing
  , _envVirtReturn = False
  }

data TopEnv = TopEnv
  { _tenvClassEnv :: Tc.ClassEnv
  , _tenvClassIds :: M.Map AST.Id Int
  , _tenvFieldEnv :: FieldMap
  }
initTopEnv :: Tc.ClassEnv -> TopEnv
initTopEnv ce = TopEnv
  { _tenvClassEnv = ce
  , _tenvClassIds = M.fromList $ zip (M.keys ce) [1..]
  , _tenvFieldEnv = M.fromList $
    [ ((className, fieldName), i)
    | className <- M.keys ce
    , let fieldsOf c = case (ce M.! c) ^. AST.super of
            Nothing -> M.keys ((ce M.! c)^.Tc.fields)
            Just sup -> fieldsOf sup ++ M.keys ((ce M.! c)^.Tc.fields)
    , (fieldName, i) <- zip (fieldsOf className) [1..]
    ]
  }

type Compiler = RWS Env [Block] St
type TopCompiler = Reader TopEnv

makeLensesWith abbreviatedFields ''Env
makeLensesWith abbreviatedFields ''St
makeLensesWith abbreviatedFields ''TopEnv

liftCompiler :: Label -> Compiler a -> St -> TopCompiler (a, [Block])
liftCompiler rl@(Label rname) act s = do
  ce <- initEnv rl (Label $ rname ++ "_init")
  return $ evalRWS act ce s

liftTop :: TopCompiler a -> Compiler a
liftTop act = do
  e <- view topEnv
  return $ runReader act e

labelFromId :: AST.Id -> Label
labelFromId = Label . AST.iName

makeConstructorName :: AST.Id -> Maybe AST.Id -> String
makeConstructorName cname = \case
  Nothing -> "cstr_" ++ AST.iName cname ++ "_unnamed_"
  Just n -> "cstr_" ++ AST.iName cname ++ "_" ++ AST.iName n

makeMethodName :: Int -> AST.Id -> String
makeMethodName ci mth = "mth_" ++ show ci ++ "_" ++ AST.iName mth

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

registerVar :: Type -> AST.Id -> Compiler VarId
registerVar t v = do
  i <- newVar t
  modify (over varMap (M.insert v i))
  return i

loadVar :: AST.Id -> Compiler VarId
loadVar i = uses varMap (maybe (error $ "no var? " ++ AST.iName i) id . M.lookup i)

localScope :: Compiler a -> Compiler a
localScope k = do
  s <- get
  r <- local (set virtReturn False) k
  modify (set varMap (s^.varMap) . set typeMap (s^.typeMap))
  return r

constrName :: AST.Id -> Maybe AST.Id -> String
constrName c mco = "cstr_" ++ AST.iName c ++ "_" ++ maybe "" AST.iName mco

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
            Nothing -> mapM cType (M.elems $ ce^.Tc.fields)
            Just x -> do
              sfs <- processFields x
              fs <- mapM cType (M.elems $ ce^.Tc.fields)
              return (sfs ++ fs)
    fs <- processFields c
    return $ TObj i fs

cExpr :: AST.Expr 'AST.Typed -> Compiler Const
cExpr = \case
  AST.ELit _ _ l -> cLit l
  AST.EVar _ _ v -> CVar <$> loadVar v
  AST.EOp _ te o l r -> do
    lv <- cExpr l
    rv <- cExpr r
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
    return $ CVar v
  AST.EUnOp _ te o e -> do
    ee <- cExpr e
    let uo = case o of
          AST.Not -> Not
          AST.Neg -> Neg
    t <- liftTop $ cType te
    v <- newVar t
    write [Assg t v (UnOp uo ee)]
    return $ CVar v
  AST.EApp _ rt f as -> do
    asRefs <- mapM cExpr as
    t <- liftTop $ cType rt
    v <- newVar t
    write $ [Assg t v (Call (AST.iName f) asRefs)]
    return $ CVar v
  AST.EProj _ t e fld -> do
    ce <- cExpr e
    tt <- liftTop $ cType t
    v <- newVar tt
    let (AST.TClass c) = AST.getExprDec e
    offset <- liftTop $ views fieldEnv (M.! (c,fld))
    write [Assg tt v (Proj ce offset)]
    return $ CVar v
  AST.EMApp _ rt e m as -> do
    ce <- cExpr e
    asRefs <- mapM cExpr as
    t <- liftTop $ cType rt
    et <- liftTop $ cType (AST.getExprDec e)
    v <- newVar t
    write $ [Assg t v (VCall (AST.iName m) (ce:asRefs))]
    return $ CVar v
  AST.ENew _ t c n as -> do
    asRefs <- mapM cExpr as
    tt <- liftTop $ cType t
    alloc <- newVar tt
    construct <- newVar tt
    write [ Assg tt alloc NewObj
          , Assg tt construct (Call (constrName c n) (CVar alloc:asRefs))
          ]
    return $ CVar construct

cCondJump :: AST.Expr 'AST.Typed -> Label -> Label -> Compiler ()
cCondJump e ltrue lfalse =
  let relCond o l r = do
        lv <- cExpr l
        rv <- cExpr r
        cutBlock (Br (Cond o lv rv) ltrue lfalse)
      naiveCond = do
        ve <- cExpr e
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
  AST.SAssg _ v e k -> do
    ve <- cExpr e
    t <- liftTop $ cType (AST.getExprDec e)
    vv <- loadVar v
    write [Assg t vv (Const ve)]
    cStmt k
  AST.SFieldAssg _ b f e k -> do
    vb <- cExpr b
    ve <- cExpr e
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
  AST.SRet _ e _ -> do
    ve <- cExpr e
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
  AST.SExp _ e@(AST.EApp _ _ "error" _) _ -> do
    void $ cExpr e
    cutBlock Unreachable
  AST.SExp _ e k -> do
    void $ cExpr e
    cStmt k
  AST.SBlock _ b k -> do
    bl <- makeLabel "block"
    cl <- makeLabel "block"
    cutBlock (Jmp bl)
    newBlockCont bl cl (localScope $ cStmt b)
    newBlock cl (cStmt k)
  AST.SEmpty _ -> do
    n <- view nextBlock
    vret <- view virtReturn
    cutBlock $ case n of
      Just nb -> Jmp nb
      Nothing -> if vret then Ret Nothing else Unreachable

compileBody :: Bool -> Label -> St -> AST.Stmt 'AST.Typed -> TopCompiler [Block]
compileBody retVoid l st b = snd <$> liftCompiler l (local (set virtReturn retVoid) (cStmt b)) st

cFunDef :: AST.FunDef 'AST.Typed -> TopCompiler Routine
cFunDef f = do
  argTypes <- mapM cType (f^.AST.args <&> (^.AST.ty))
  let argIds = map (VarId . negate) [1..length $ f^.AST.args]
      initVarMap = M.fromList $ zip (f^.AST.args <&> (^.AST.name)) argIds
      initTypeMap = M.fromList $ zip argIds argTypes
  body <- compileBody (f^.AST.retType == AST.TVoid) (labelFromId $ f^.AST.name)
             (initSt initVarMap initTypeMap) (f^.AST.body)
  return $  Routine (labelFromId $ f^.AST.name) argIds body

cTopDef :: AST.TopDef 'AST.Typed -> TopCompiler [Routine]
cTopDef = \case
  AST.TDFun f -> pure <$> cFunDef f
  AST.TDClass c ->
    fmap concat $ forM (c^.AST.body) $ \case
    AST.CMMethod mth -> cMethod (c^.AST.name) mth

cMethod :: AST.Id -> AST.Method 'AST.Typed -> TopCompiler [Routine]
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
      labelName = Label $ makeMethodName classId (mth^.AST.name)
  r <- compileBody (mth^.AST.retType == AST.TVoid) labelName
       (initSt initVarMap initTypeMap) (methodBody)
  return [Routine labelName argIds r]
cMethod _ _ = return []

buildMethodMap :: TopCompiler MethodMap
buildMethodMap = do
  cenv <- view classEnv
  let classes = M.toList cenv
      allMethods = nub [m | (_, c) <- classes, m <- M.keys (c ^. Tc.methods)]
  flip execStateT [] $ forM_ allMethods $ \m -> do
    impls <- forM classes $ \(cn, _) -> do
      let search :: AST.Id -> Maybe AST.Id
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


getUsedVars :: Block -> S.Set VarId
getUsedVars (Block _ instrs _) = S.fromList $ [v | Assg _ v _ <- instrs]


instance Pretty Type where
  pPrint = \case
    TInt i -> "int" <> int i
    TString -> "string"
    TVoid -> "void"
    TObj i _ -> "obj:" <> int i

instance Pretty Label where
  pPrint (Label s) = text s

instance IsString Label where
  fromString = Label

instance Pretty VarId where
  pPrint (VarId v) = "%" <> int v

instance Pretty Const where
  pPrint = \case
    CVar v -> pPrint v
    CInt i -> integer i
    CStr s -> pPrint s

instance Pretty UnOp where
  pPrint = \case
    Not -> "NOT"
    Neg -> "NEG"

instance Pretty NumOp where
  pPrint = \case
    Add -> "ADD"
    Sub -> "SUB"
    Mul -> "MUL"
    Div -> "DIV"
    Mod -> "MOD"
    And -> "AND"
    Or  -> "OR"

instance Pretty RelOp where
  pPrint = \case
    Eq  -> "EQ"
    Neq -> "NEQ"
    Lt  -> "LT"
    Le  -> "LE"
    Gt  -> "GT"
    Ge  -> "GE"

instance Pretty Expr where
  pPrint = \case
    UnOp o v -> pPrint o <+> pPrint v
    NumOp o l r -> pPrint o <+> pPrint l <+> pPrint r
    RelOp o l r -> pPrint o <+> pPrint l <+> pPrint r
    Const c -> pPrint c
    Call f as -> text f <> parens (cat $ punctuate comma $ map pPrint as)
    VCall f as -> "vrt " <> text f <> parens (cat $ punctuate comma $ map pPrint as)
    NewObj -> "new_obj"
    Proj a i -> "π_" <> int i <+> pPrint a

instance Pretty Cond where
  pPrint = \case
    Cond o l r -> parens (pPrint o <+> pPrint l <+> pPrint r)
    CondConst c -> pPrint c

instance Pretty FinInstr where
  pPrint = \case
    Ret c -> "RET" <+> maybe "void" pPrint c
    Jmp i -> "JMP" <+> pPrint i
    Br c i1 i2 -> "BR" <+> pPrint c <+> pPrint i1 <+> pPrint i2
    Unreachable -> "*unreachable*"

instance Pretty Instr where
  pPrint = \case
    Assg t i e -> pPrint t <+> pPrint i <+> "=" <+> pPrint e
    FieldAssg t b f e ->  pPrint t <+> "π_" <> int f <+> pPrint b <+> "=" <+> pPrint e

instance Pretty Block where
  pPrint (Block i is f) =
    pPrint i <> ":" $+$ nest 2 ((vcat (map pPrint is)) $+$ pPrint f)

instance Pretty Routine where
  pPrint (Routine i as b) =
    "@" <> pPrint i <> parens (cat $ punctuate comma $ map pPrint as) <> ":" $+$ nest 2 (vcat (map pPrint b))

instance Pretty IR where
  pPrint (IR rs) = vcat (map pPrint rs)

