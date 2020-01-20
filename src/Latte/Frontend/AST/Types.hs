{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Latte.Frontend.AST.Types where

import Data.String
import Data.List.NonEmpty(NonEmpty)
import qualified Data.List.NonEmpty as NE
import GHC.TypeNats(Nat, type (+))
import Control.Lens

import Latte.Pretty

import Prelude hiding ((<>), EQ, GT, LT)


data Lit
  = LInt Integer
  | LString String
  | LBool Bool
  deriving (Show)


newtype VarId = VarId {_vidIdStr :: String} deriving (Eq, Ord, Show)
newtype FunId = FunId {_fnidIdStr :: String} deriving (Eq, Ord, Show)
newtype ClassId = ClassId {_clidIdStr :: String} deriving (Eq, Ord, Show)
newtype MethodId = MethodId {_midIdStr :: String} deriving (Eq, Ord, Show)
newtype FieldId = FieldId {_flidIdStr :: String} deriving (Eq, Ord, Show)
newtype ConstructorId = ConstructorId {_ctidIdStr :: String} deriving (Eq, Ord, Show)


data Ann = Ann
  { _annFile :: FilePath
  , _annLine :: Int
  , _annColumn :: Int
  } deriving (Eq)
fakeAnn :: Ann
fakeAnn = Ann "<no location>" 0 0


data Type
  = TVoid
  | TInt
  | TBool
  | TString
  | TClass ClassId
  deriving (Eq)


data Arg = Arg
  { _argAnn :: Ann
  , _argTy :: Type
  , _argName :: VarId
  }

type Args = [Arg]


data OpType = Rel | Add | Mul | Log

data Op (t :: OpType) where
  LT    :: Ann -> Op 'Rel
  LEQ   :: Ann -> Op 'Rel
  EQ    :: Ann -> Op 'Rel
  NEQ   :: Ann -> Op 'Rel
  GEQ   :: Ann -> Op 'Rel
  GT    :: Ann -> Op 'Rel
  Plus  :: Ann -> Op 'Add
  Minus :: Ann -> Op 'Add
  Mult  :: Ann -> Op 'Mul
  Div   :: Ann -> Op 'Mul
  Mod   :: Ann -> Op 'Mul
  Or    :: Ann -> Op 'Log
  And   :: Ann -> Op 'Log
deriving instance Eq (Op t)


data AnyOp where
  Op :: Op t -> AnyOp


data UnOp
  = Neg
  | Not


data RawExpr (l :: Nat) where
  REOr    :: Ann -> RawExpr 1 -> RawExpr 0                  -> RawExpr 0
  REAnd   :: Ann -> RawExpr 2 -> RawExpr 1                  -> RawExpr 1
  RERelOp :: Ann -> Op 'Rel -> RawExpr 2 -> RawExpr 3       -> RawExpr 2
  REAddOp :: Ann -> Op 'Add -> RawExpr 3 -> RawExpr 4       -> RawExpr 3
  REMulOp :: Ann -> Op 'Mul -> RawExpr 4 -> RawExpr 5       -> RawExpr 4
  RENot   :: Ann -> RawExpr 6                               -> RawExpr 5
  RENeg   :: Ann -> RawExpr 6                               -> RawExpr 5
  REProj  :: Ann -> RawExpr 6 -> FieldId                    -> RawExpr 6
  REMApp  :: Ann -> RawExpr 6 -> MethodId -> [RawExpr 0]    -> RawExpr 6
  RELit   :: Ann -> Lit                                     -> RawExpr 7
  REApp   :: Ann -> FunId   -> [RawExpr 0]                  -> RawExpr 7
  RENew   :: Ann -> ClassId -> Maybe ConstructorId -> [RawExpr 0] -> RawExpr 7
  REVar   :: Ann -> VarId                                   -> RawExpr 7
  REPar   :: Ann -> RawExpr 0                               -> RawExpr 7
  RESuper :: Ann                                            -> RawExpr 7
  RECoe   ::        RawExpr (n + 1)                         -> RawExpr n

type E = RawExpr 0


data RawStmt
  = RSAssg Ann VarId E
  | RSFieldAssg Ann E FieldId E
  | RSDecl Ann Type (NonEmpty (VarId, Maybe E))
  | RSIncr Ann VarId
  | RSDecr Ann VarId
  | RSRet Ann E
  | RSVRet Ann
  | RSCond Ann E RawStmt
  | RSCondElse Ann E RawStmt RawStmt
  | RSWhile Ann E RawStmt
  | RSExp Ann E
  | RSBlock Ann [RawStmt]
  | RSEmpty Ann


data Stage = Untyped | Typed


type family ExprDecoration (s :: Stage) where
  ExprDecoration 'Untyped = ()
  ExprDecoration 'Typed = Type


data Expr (s :: Stage) where
  ELit   :: Ann -> ExprDecoration s -> Lit -> Expr s
  EApp   :: Ann -> ExprDecoration s -> FunId -> [Expr s] -> Expr s
  EVar   :: Ann -> ExprDecoration s -> VarId -> Expr s
  EUnOp  :: Ann -> ExprDecoration s -> UnOp -> Expr s -> Expr s
  EOp    :: Ann -> ExprDecoration s -> AnyOp -> Expr s -> Expr s -> Expr s
  EProj  :: Ann -> ExprDecoration s -> Expr s -> FieldId -> Expr s
  EMApp  :: Ann -> ExprDecoration s -> Expr s -> MethodId -> [Expr s] -> Expr s
  ENew   :: Ann -> ExprDecoration s -> ClassId -> Maybe ConstructorId -> [Expr s] -> Expr s
  ESuper :: Ann -> ExprDecoration s -> Expr s

getExprDec :: Expr s -> ExprDecoration s
getExprDec = \case
  ELit _ a _ -> a
  EVar _ a _ -> a
  EApp _ a _ _ -> a
  EUnOp _ a _ _ -> a
  EOp  _ a _ _ _ -> a
  EProj _ a _ _ -> a
  EMApp _ a _ _ _ -> a
  ENew _ a _ _ _ -> a
  ESuper _ a -> a


data Stmt (e :: Stage)
  = SAssg Ann VarId (Expr e) (Stmt e)
  | SFieldAssg Ann (Expr e) FieldId (Expr e) (Stmt e)
  | SDecl Ann Type VarId (Stmt e)
  | SIncr Ann VarId (Stmt e)
  | SDecr Ann VarId (Stmt e)
  | SRet Ann (Expr e) (Stmt e)
  | SVRet Ann (Stmt e)
  | SCond Ann (Expr e) (Stmt e) (Stmt e)
  | SCondElse Ann (Expr e) (Stmt e) (Stmt e) (Stmt e)
  | SWhile Ann (Expr e) (Stmt e) (Stmt e)
  | SExp Ann (Expr e) (Stmt e)
  | SBlock Ann (Stmt e) (Stmt e)
  | SEmpty Ann


data FunDef (s :: Stage) = FunDef
  { _fundefAnn :: Ann
  , _fundefRetType :: Type
  , _fundefName :: FunId
  , _fundefArgs :: [Arg]
  , _fundefBody :: Stmt s
  }


data ClassDef (s :: Stage) = ClassDef
  { _classdefAnn :: Ann
  , _classdefName :: ClassId
  , _classdefSuper :: Maybe ClassId
  , _classdefBody :: [ClassMember s]
  }


data TopDef (s :: Stage)
  = TDFun (FunDef s)
  | TDClass (ClassDef s)


data Method (s :: Stage) = Method
  { _methodAnn :: Ann
  , _methodAccess :: ClassMemberAccess
  , _methodPlace :: ClassMemberPlace
  , _methodRetType :: Type
  , _methodName :: MethodId
  , _methodArgs :: [Arg]
  , _methodBody :: Maybe (Stmt s)
  }


data Field (s :: Stage) = Field
  { _fieldAnn :: Ann
  , _fieldAccess :: ClassMemberAccess
  , _fieldPlace :: ClassMemberPlace
  , _fieldTy :: Type
  , _fieldAssignments :: NonEmpty (FieldId, Maybe (Expr s))
  }


data Constructor (s :: Stage) = Constructor
  { _constructorAnn :: Ann
  , _constructorAccess :: ClassMemberAccess
  , _constructorName :: Maybe ConstructorId
  , _constructorArgs :: [Arg]
  , _constructorBody :: Stmt s
  }


data ClassMember (s :: Stage)
  = CMMethod (Method s)
  | CMField (Field s)
  | CMConstructor (Constructor s)


data ClassMemberPlace = Dynamic | Static
  deriving (Show)


data ClassMemberAccess = Private | Public | Protected
  deriving (Show)


newtype Program (s :: Stage) = Program [TopDef s]


makeLensesWith abbreviatedFields ''VarId
makeLensesWith abbreviatedFields ''FunId
makeLensesWith abbreviatedFields ''ClassId
makeLensesWith abbreviatedFields ''FieldId
makeLensesWith abbreviatedFields ''MethodId
makeLensesWith abbreviatedFields ''ConstructorId
makeLensesWith abbreviatedFields ''Ann
makeLensesWith abbreviatedFields ''Arg
makeLensesWith abbreviatedFields ''Type
makeLensesWith abbreviatedFields ''FunDef
makeLensesWith abbreviatedFields ''ClassDef
makeLensesWith abbreviatedFields ''Method
makeLensesWith abbreviatedFields ''Field
makeLensesWith abbreviatedFields ''Constructor


class (HasIdStr a String, Ord a, Eq a) => IsId a where
instance (HasIdStr a String, Ord a, Eq a) => IsId a where

instance IsString VarId where
  fromString = VarId

instance IsString FunId where
  fromString = FunId

instance IsString ClassId where
  fromString = ClassId

instance IsString FieldId where
  fromString = FieldId

instance IsString MethodId where
  fromString = MethodId

instance IsString ConstructorId where
  fromString = ConstructorId

instance Pretty VarId where
  pPrint i = text $ i^.idStr

instance Pretty FunId where
  pPrint i = text $ i^.idStr

instance Pretty ClassId where
  pPrint i = text $ i^.idStr

instance Pretty FieldId where
  pPrint i = text $ i^.idStr

instance Pretty MethodId where
  pPrint i = text $ i^.idStr

instance Pretty ConstructorId where
  pPrint i = text (i^.idStr)


instance Pretty Arg where
  pPrint (Arg _ t i) = pPrint t <+> pPrint i


instance Pretty Lit where
  pPrint = \case
    LInt i -> integer i
    LString s -> doubleQuotes $ text s
    LBool True -> "true"
    LBool False -> "false"


instance Pretty Ann where
  pPrint a = text (a^.file) <> ":" <> int (a^.line) <> ":" <> int (a^.column)


instance Pretty (Op t) where
  pPrint = \case
    LT _    -> "<"
    LEQ _   -> "<="
    EQ _    -> "=="
    NEQ _   -> "!="
    GEQ _   -> ">="
    GT _    -> ">"
    Plus _  -> "+"
    Minus _ -> "-"
    Mult _  -> "*"
    Div _   -> "/"
    Mod _   -> "%"
    Or _    -> "||"
    And _   -> "&&"


instance Pretty AnyOp where
  pPrint (Op o) = pPrint o


instance Pretty UnOp where
  pPrint = \case
    Neg -> "-"
    Not -> "!"


instance Pretty Type where
  pPrint = \case
    TVoid -> "void"
    TInt -> "int"
    TBool -> "boolean"
    TString -> "string"
    TClass i -> pPrint i


instance Pretty (Expr a) where
  pPrintPrec k prc = \case
    ELit _ _ l -> pPrint l
    EApp _ _ f as -> pPrint f <> parens (cat $ punctuate comma $ map pPrint as)
    EVar _ _ v -> pPrint v
    EUnOp _ _ o e -> pPrint o <> pPrint e
    EOp  _ _ o l r ->
      let pri :: AnyOp -> Rational
          pri = \case
            Op (Plus _)  -> 6
            Op (Minus _) -> 6
            Op (Mult _)  -> 7
            Op (Div _)   -> 7
            Op (Mod _)   -> 7
            Op (LT _)    -> 8
            Op (LEQ _)   -> 8
            Op (EQ _)    -> 8
            Op (NEQ _)   -> 8
            Op (GEQ _)   -> 8
            Op (GT _)    -> 8
            Op (And _)   -> 9
            Op (Or _)    -> 10
      in maybeParens (prc >= pri o) $ pPrintPrec k (pri o) l <+> pPrint o <+> pPrintPrec k (pri o) r
    EProj _ _ e i -> pPrint e <> "." <> pPrint i
    EMApp _ _ e m as -> pPrint e <> "." <> pPrint m <>
                           parens (cat $ punctuate comma $ map pPrint as)
    ENew _ _ cl cr as -> "new" <+> pPrint cl <> maybe empty (\n -> "." <> pPrint n) cr <>
      parens (cat $ punctuate comma $ map pPrint as)
    ESuper _ _ -> "super"


block :: Doc -> Doc
block b = lbrace $+$ nest 2 b $+$ rbrace


instance Pretty (Stmt a) where
  pPrint = \case
    SAssg _ v e cont -> pPrint v <+> "=" <+> pPrint e <> semi $+$ pPrint cont
    SFieldAssg _ b v e cont -> pPrint b <> "." <> pPrint v <+> "=" <+> pPrint e <> semi $+$ pPrint cont
    SDecl _ t v cont -> pPrint t <+> pPrint v <> semi $+$ pPrint cont
    SIncr _ v cont -> pPrint v <> "++" <> semi $+$ pPrint cont
    SDecr _ v cont -> pPrint v <> "--" <> semi $+$ pPrint cont
    SRet _ e cont -> "return" <+> pPrint e <> semi $+$ pPrint cont
    SVRet _ cont -> "return" <> semi $+$ pPrint cont
    SCond _ c t cont -> "if" <> parens (pPrint c) <+> block (pPrint t) $+$ pPrint cont
    SCondElse _ c t e cont -> "if" <> parens (pPrint c) <+> block (pPrint t) $+$
                              "else" <+> block (pPrint e) $+$
                              pPrint cont
    SWhile _ c b cont -> "while" <> parens (pPrint c) <+> block (pPrint b) $+$ pPrint cont
    SExp _ e cont -> pPrint e $+$ pPrint cont
    SBlock _ b cont -> block (pPrint b) $+$ pPrint cont
    SEmpty _ -> empty


instance Pretty (FunDef a) where
  pPrint fd = pPrint (fd^.retType) <+> pPrint (fd^.name) <> parens (cat $ punctuate comma $ map pPrint $ fd^.args) <+>
              block (pPrint (fd^.body))


instance Pretty (TopDef a) where
  pPrint = \case
    TDFun f -> pPrint f
    TDClass c -> pPrint c


instance Pretty (ClassDef a) where
  pPrint c = "class" <+> pPrint (c^.name) <+> maybe empty (("extends" <+>) . pPrint) (c^.super) <+>
    block ( vcat $ map pPrint (c^.body))


instance Pretty (ClassMember a) where
  pPrint = \case
    CMMethod m -> pPrint m
    CMField f -> pPrint f
    CMConstructor c -> pPrint c


instance Pretty (Method a) where
  pPrint md =
    pPrint (md^.place) <+> pPrint (md^.access) <+>
    pPrint (md^.retType) <+> pPrint (md^.name) <>
    parens (cat $ punctuate comma $ map pPrint $ md^.args) <+>
    block (maybe empty pPrint (md^.body))


instance Pretty (Field a) where
  pPrint f =
    pPrint (f^.place) <+> pPrint (f^.access) <+>
    pPrint (f^.ty) <+>
    cat (punctuate comma (map (\(i, mv) ->
                                  pPrint i <+> maybe empty (\v -> "=" <+> pPrint v) mv)
                           (NE.toList $ f^.assignments))) <>
    semi

instance Pretty (Constructor a) where
  pPrint c =
    "new" <+>
    pPrint (c^.name) <>
    parens (cat $ punctuate comma $ map pPrint $ c^.args) <+>
    block (pPrint (c^.body))

instance Pretty ClassMemberPlace where
  pPrint = \case
    Dynamic -> empty
    Static -> "static"


instance Pretty ClassMemberAccess where
  pPrint = \case
    Private   -> "private"
    Public    -> "public"
    Protected -> "protected"


instance Pretty (Program a) where
  pPrint (Program defs) = vcat $ map pPrint defs


instance HasAnn (Expr p) Ann where
  ann f = \case
    ELit a dec l -> fmap (\a2 -> ELit a2 dec l) (f a)
    EVar a dec v -> fmap (\a2 -> EVar a2 dec v) (f a)
    EApp a dec fname as -> fmap (\a2 -> EApp a2 dec fname as) (f a)
    EUnOp a dec o v -> fmap (\a2 -> EUnOp a2 dec o v) (f a)
    EOp a dec o l r -> fmap (\a2 -> EOp a2 dec o l r) (f a)
    EProj a dec e i -> fmap (\a2 -> EProj a2 dec e i) (f a)
    EMApp a dec e i as -> fmap (\a2 -> EMApp a2 dec e i as) (f a)
    ENew a dec c i as -> fmap (\a2 -> ENew a2 dec c i as) (f a)
    ESuper a dec -> fmap (\a2 -> ESuper a2 dec) (f a)
