{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Latte.Frontend.IR where

import qualified Latte.Frontend.AST as AST

import Text.PrettyPrint.HughesPJClass
import Data.String
import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.RWS hiding ((<>))
import Control.Monad.Identity
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

data FinInstr
  = Ret (Maybe Const)
  | Jmp Label
  | Br Cond Label Label

data Cond = Cond RelOp Const Const | CondConst Const

data Expr
  = NumOp NumOp Const Const
  | RelOp RelOp Const Const
  | UnOp UnOp Const
  | Const Const
  | Call String [Const] -- result fname args

data UnOp = Neg | Not
data NumOp = Add | Sub | Mul | Div | Mod | Or | And
data RelOp = Eq | Neq | Lt | Le | Gt | Ge

data Const = CVar VarId | CInt Integer | CStr String

type VarMap = M.Map AST.Id VarId
type TypeMap = M.Map VarId Type

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
  , _envBlockName :: Label
  , _envNextBlock :: Label
  }
initEnv :: Label -> Label -> Env
initEnv r l = Env r l (error "Missing continuation")

type Compiler = RWS Env [Block] St

makeLensesWith abbreviatedFields ''Env
makeLensesWith abbreviatedFields ''St

labelFromId :: AST.Id -> Label
labelFromId = Label . AST.iName

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

registerVar :: Type -> AST.Id -> Compiler ()
registerVar t v = do
  i <- newVar t
  modify (over varMap (M.insert v i))

loadVar :: AST.Id -> Compiler VarId
loadVar i = uses varMap (maybe (error $ "no var? " ++ AST.iName i) id . M.lookup i)

localScope :: Compiler a -> Compiler a
localScope k = do
  s <- get
  r <- k
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

cType :: Monad m => AST.Type -> m Type
cType = \case
  AST.TInt -> pure (TInt 32)
  AST.TBool -> pure (TInt 1)
  AST.TString -> pure TString
  AST.TVoid -> pure TVoid

cExpr :: AST.Expr 'AST.Typed -> Compiler Const
cExpr = \case
  AST.ELit _ _ l -> cLit l
  AST.EVar _ _ v -> CVar <$> loadVar v
  AST.EOp _ te o l r -> do
    lv <- cExpr l
    rv <- cExpr r
    t <- cType te
    v <- newVar t
    write $ case o of
      AST.Op (AST.Plus _)  -> case te of
        AST.TString -> [Assg t v (Call "strcat" [lv, rv])]
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
    t <- cType te
    v <- newVar t
    write [Assg t v (UnOp uo ee)]
    return $ CVar v
  AST.EApp _ rt f as -> do
    asRefs <- mapM cExpr as
    t <- cType rt
    v <- newVar t
    write $ [Assg t v (Call (AST.iName f) asRefs)]
    return $ CVar v

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
newBlockCont i c = local (set nextBlock c) . newBlock i

cStmt :: AST.Stmt 'AST.Typed -> Compiler ()
cStmt = \case
  AST.SAssg _ v e k -> do
    ve <- cExpr e
    t <- cType (AST.getExprDec e)
    vv <- loadVar v
    write [Assg t vv (Const ve)]
    cStmt k
  AST.SDecl _ t v k -> do
    tt <- cType t
    registerVar tt v
    cStmt k
  AST.SIncr _ v k -> do
    t <- cType AST.TInt
    vv <- loadVar v
    write [Assg t vv (NumOp Add (CVar vv) (CInt 1))]
    cStmt k
  AST.SDecr _ v k -> do
    t <- cType AST.TInt
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
    cutBlock (Jmp n)

compileBody :: Env -> St -> AST.Stmt 'AST.Typed -> [Block]
compileBody en st b = snd $ evalRWS (cStmt b) en st

cFunDef :: AST.FunDef 'AST.Typed -> Routine
cFunDef f =
  let argTypes = runIdentity $ mapM cType (f^.AST.args <&> (^.AST.ty))
      argIds = map (VarId . negate) [1..length $ f^.AST.args]
      initVarMap = M.fromList $ zip (f^.AST.args <&> (^.AST.name)) argIds
      initTypeMap = M.fromList $ zip argIds argTypes
      body = compileBody (initEnv (labelFromId $ f^.AST.name) (Label $ (AST.iName $ f^.AST.name) ++ "_init"))
             (initSt initVarMap initTypeMap) (f^.AST.body)
  in Routine (labelFromId $ f^.AST.name) argIds body

cTopDef :: AST.TopDef 'AST.Typed -> [Routine]
cTopDef = \case
  AST.TDFun f -> [cFunDef f]

compile :: AST.Program 'AST.Typed -> IR
compile (AST.Program ts) = IR $ concatMap cTopDef ts


getUsedVars :: Block -> S.Set VarId
getUsedVars (Block _ instrs _) = S.fromList $ instrs <&> \case
  Assg _ v _ -> v


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

instance Pretty Cond where
  pPrint = \case
    Cond o l r -> parens (pPrint o <+> pPrint l <+> pPrint r)
    CondConst c -> pPrint c

instance Pretty FinInstr where
  pPrint = \case
    Ret c -> "RET" <+> maybe "void" pPrint c
    Jmp i -> "JMP" <+> pPrint i
    Br c i1 i2 -> "BR" <+> pPrint c <+> pPrint i1 <+> pPrint i2

instance Pretty Instr where
  pPrint = \case
    Assg t i e -> pPrint t <+> pPrint i <+> "=" <+> pPrint e

instance Pretty Block where
  pPrint (Block i is f) =
    pPrint i <> ":" $+$ nest 2 ((vcat (map pPrint is)) $+$ pPrint f)

instance Pretty Routine where
  pPrint (Routine i as b) =
    "@" <> pPrint i <> parens (cat $ punctuate comma $ map pPrint as) <> ":" $+$ nest 2 (vcat (map pPrint b))

instance Pretty IR where
  pPrint (IR rs) = vcat (map pPrint rs)

