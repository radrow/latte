{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Latte.Frontend.IR.Types where

import Latte.Pretty
import qualified Latte.Frontend.AST as AST
import qualified Latte.Frontend.Typechecker as Tc

import Data.String
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.RWS hiding ((<>))
import Control.Monad.Reader
import Control.Lens hiding (Const)
import Prelude hiding ((<>))


data VarId = VarId {vId :: Int} deriving (Ord, Eq)


data Label = Label {lId :: String} deriving (Ord, Eq)


data Type = TInt Int | TString | TVoid | TObj Int [Type] | TCId


data IR = IR [Routine]


data Routine = Routine Bool Label [VarId] [Block]


data Block = Block Label [Instr] FinInstr


data Instr
  = Assg Type VarId Expr
  | FieldAssg Type Const Int Expr


data FinInstr
  = Ret (Maybe Const)
  | Jmp Label
  | Br Cond Label Label
  | TailCall String [Const] -- result fname args
  | TailVCall String [Const]
  | Unreachable


data Cond = Cond RelOp Const Const | CondConst Const


data Expr
  = NumOp NumOp Const Const
  | RelOp RelOp Const Const
  | UnOp UnOp Const
  | Const Const
  | Call String [Const]
  | VCall String Const Const [Const]
  | NewObj
  | Proj Const Int
  | GetCId Const


data UnOp = Neg | Not
data NumOp = Add | Sub | Mul | Div | Mod | Or | And
data RelOp = Eq | Neq | Lt | Le | Gt | Ge


data Const = CVar VarId | CInt Integer | CStr String


type VarMap = M.Map AST.VarId VarId
type TypeMap = M.Map VarId Type
type FieldMap = M.Map (AST.ClassId, AST.FieldId) Int
type MethodMap = [(AST.MethodId, M.Map String [Int])]  -- [(method, implementation -> classes)]


data St = St
  { _stSupply :: Int
  , _stCurrentBlock :: [Instr]
  , _stVarMap :: VarMap
  , _stTypeMap :: TypeMap
  }
initSt :: VarMap -> TypeMap -> St
initSt vm tm = St 0 [] vm tm


data Env = Env
  { _envRoutineLabel   :: Label
  , _envBlockName      :: Label
  , _envNextBlock      :: Maybe Label
  , _envTopEnv         :: TopEnv
  , _envCurrentClass   :: Maybe AST.ClassId
  , _envCurrentRoutine :: Maybe String
  , _envDefaultRet     :: Maybe (Maybe Const)
  , _envTailCall       :: Bool
  }
initEnv :: Label -> Label -> TopCompiler Env
initEnv r l = ask >>= \te -> pure Env
  { _envRoutineLabel   = r
  , _envBlockName      = l
  , _envNextBlock      = Nothing
  , _envTopEnv         = te
  , _envCurrentClass   = Nothing
  , _envCurrentRoutine = Nothing
  , _envDefaultRet     = Nothing
  , _envTailCall       = False
  }


data TopEnv = TopEnv
  { _tenvClassEnv :: Tc.ClassEnv
  , _tenvClassIds :: M.Map AST.ClassId Int
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


getUsedVars :: Block -> S.Set VarId
getUsedVars (Block _ instrs _) = S.fromList $ [v | Assg _ v _ <- instrs]


instance Pretty Type where
  pPrint = \case
    TInt i -> "int" <> int i
    TString -> "string"
    TVoid -> "void"
    TObj i _ -> "obj:" <> int i
    TCId -> "classID"


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
    VCall f cid ob as -> pPrint cid <> ":" <> text f <> parens (cat $ punctuate comma $ map pPrint (ob:as))
    NewObj -> "new_obj"
    Proj a i -> "π_" <> int i <+> pPrint a
    GetCId o -> "classID" <+> pPrint o


instance Pretty Cond where
  pPrint = \case
    Cond o l r -> parens (pPrint o <+> pPrint l <+> pPrint r)
    CondConst c -> pPrint c


instance Pretty FinInstr where
  pPrint = \case
    Ret c -> "RET" <+> maybe "void" pPrint c
    Jmp i -> "JMP" <+> pPrint i
    Br c i1 i2 -> "BR" <+> pPrint c <+> pPrint i1 <+> pPrint i2
    TailCall f as ->
      "tail" <+> text f <> parens (cat $ punctuate comma $ map pPrint as)
    TailVCall f as ->
      "tail" <+> "vrt" <+> text f <> parens (cat $ punctuate comma $ map pPrint as)
    Unreachable -> "*unreachable*"


instance Pretty Instr where
  pPrint = \case
    Assg t i e -> pPrint t <+> pPrint i <+> "=" <+> pPrint e
    FieldAssg t b f e ->  pPrint t <+> "π_" <> int f <+> pPrint b <+> "=" <+> pPrint e


instance Pretty Block where
  pPrint (Block i is f) =
    pPrint i <> ":" $+$ nest 2 ((vcat (map pPrint is)) $+$ pPrint f)


instance Pretty Routine where
  pPrint (Routine virt i as b) =
    (if virt then "V@" else "@") <> pPrint i <> parens (cat $ punctuate comma $ map pPrint as) <>
    ":" $+$ nest 2 (vcat (map pPrint b))


instance Pretty IR where
  pPrint (IR rs) = vcat (map pPrint rs)

