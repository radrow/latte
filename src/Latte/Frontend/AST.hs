{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Latte.Frontend.AST where

import Data.String
import Data.List.NonEmpty(NonEmpty)
import qualified Data.List.NonEmpty as NE
import Text.PrettyPrint.HughesPJClass
import GHC.TypeNats(Nat, type (+))
import Control.Lens

import Prelude hiding ((<>), EQ, GT, LT)


data Lit
  = LInt Integer
  | LString String
  | LBool Bool
  deriving (Show)

newtype Id = Id {iName :: String}
  deriving (Eq, Ord)


data Ann = Ann
  { _annFile :: FilePath
  , _annLine :: Int
  , _annColumn :: Int
  }

fakeAnn :: Ann
fakeAnn = Ann "no_file" 0 0


data Type
  = TVoid
  | TInt
  | TBool
  | TString
  | TClass Id


data Arg = Arg
  { _argAnn :: Ann
  , _argTy :: Type
  , _argName :: Id
  }

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

data AnyOp where
  Op :: Op t -> AnyOp

instance Pretty AnyOp where
  pPrint (Op o) = pPrint o


data UnOp
  = Neg
  | Not

instance Pretty UnOp where
  pPrint = \case
    Neg -> "-"
    Not -> "!"


data RawExpr (l :: Nat) where
  REOr    :: Ann -> RawExpr 1 -> RawExpr 0            -> RawExpr 0
  REAnd   :: Ann -> RawExpr 2 -> RawExpr 1            -> RawExpr 1
  RERelOp :: Ann -> Op 'Rel -> RawExpr 2 -> RawExpr 3 -> RawExpr 2
  REAddOp :: Ann -> Op 'Add -> RawExpr 3 -> RawExpr 4 -> RawExpr 3
  REMulOp :: Ann -> Op 'Mul -> RawExpr 4 -> RawExpr 5 -> RawExpr 4
  RENot   :: Ann -> RawExpr 6                         -> RawExpr 5
  RENeg   :: Ann -> RawExpr 6                         -> RawExpr 5
  REProj  :: Ann -> RawExpr 6 -> Id                   -> RawExpr 6
  REMApp  :: Ann -> RawExpr 6 -> Id -> [RawExpr 0]    -> RawExpr 6
  RELit   :: Ann -> Lit                               -> RawExpr 7
  REApp   :: Ann -> Id -> [RawExpr 0]                 -> RawExpr 7
  RENew   :: Ann -> Id -> Maybe Id -> [RawExpr 0]     -> RawExpr 7
  REVar   :: Ann -> Id                                -> RawExpr 7
  REPar   :: Ann -> RawExpr 0                         -> RawExpr 7
  RECoe   ::        RawExpr (n + 1)                   -> RawExpr n

type E = RawExpr 0


data RawStmt
  = RSAssg Ann Id E
  | RSFieldAssg Ann E Id E
  | RSDecl Ann Type (NonEmpty (Id, Maybe E))
  | RSIncr Ann Id
  | RSDecr Ann Id
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
  ELit  :: Ann -> ExprDecoration s -> Lit -> Expr s
  EApp  :: Ann -> ExprDecoration s -> Id -> [Expr s] -> Expr s
  EVar  :: Ann -> ExprDecoration s -> Id -> Expr s
  EUnOp :: Ann -> ExprDecoration s -> UnOp -> Expr s -> Expr s
  EOp   :: Ann -> ExprDecoration s -> AnyOp -> Expr s -> Expr s -> Expr s
  EProj :: Ann -> ExprDecoration s -> Expr s -> Id -> Expr s
  EMApp :: Ann -> ExprDecoration s -> Expr s  -> Id -> [Expr s] -> Expr s
  ENew :: Ann -> ExprDecoration s -> Id -> Maybe Id -> [Expr s] -> Expr s


data Stmt (e :: Stage)
  = SAssg Ann Id (Expr e) (Stmt e)
  | SFieldAssg Ann (Expr e) Id (Expr e) (Stmt e)
  | SDecl Ann Type Id (Stmt e)
  | SIncr Ann Id (Stmt e)
  | SDecr Ann Id (Stmt e)
  | SRet Ann (Expr e) (Stmt e)
  | SVRet Ann (Stmt e)
  | SCond Ann (Expr e) (Stmt e) (Stmt e)
  | SCondElse Ann (Expr e) (Stmt e) (Stmt e) (Stmt e)
  | SWhile Ann (Expr e) (Stmt e) (Stmt e)
  | SExp Ann (Expr e) (Stmt e)
  | SBlock Ann (Stmt e) (Stmt e)
  | SEmpty Ann


entailExpr :: RawExpr t -> Expr 'Untyped
entailExpr = \case
  REOr    ann e1 e2 -> EOp ann () (Op $ Or ann)  (entailExpr e1) (entailExpr e2)
  REAnd   ann e1 e2 -> EOp ann () (Op $ And ann) (entailExpr e1) (entailExpr e2)
  RERelOp ann o e1 e2 -> EOp ann () (Op o) (entailExpr e1) (entailExpr e2)
  REAddOp ann o e1 e2 -> EOp ann () (Op o) (entailExpr e1) (entailExpr e2)
  REMulOp ann o e1 e2 -> EOp ann () (Op o) (entailExpr e1) (entailExpr e2)
  RENeg   ann e -> EUnOp ann () Neg (entailExpr e)
  RENot   ann e -> EUnOp ann () Not (entailExpr e)
  RELit   ann l -> ELit ann () l
  REApp   ann f args -> EApp ann () f (map entailExpr args)
  RENew   ann c f args -> ENew ann () c f (map entailExpr args)
  REVar   ann v -> EVar ann () v
  REPar   _ e -> entailExpr e
  RECoe   e -> entailExpr e
  REProj  ann e i -> EProj ann () (entailExpr e) i
  REMApp  ann e i as -> EMApp ann () (entailExpr e) i (map entailExpr as)

entailStmt :: RawStmt -> Stmt 'Untyped
entailStmt s =
  let entailSingle :: RawStmt -> Stmt 'Untyped -> Stmt 'Untyped
      entailSingle = \case
        RSAssg ann i v -> SAssg ann i (entailExpr v)
        RSFieldAssg ann e i v -> SFieldAssg ann (entailExpr e) i (entailExpr v)
        RSDecl ann t vs ->
          foldr (\(i, mval) lk ->
                   SDecl ann t i .
                   case mval of
                     Nothing -> lk
                     Just e -> SAssg ann i (entailExpr e) . lk
                ) id vs
        RSIncr ann i -> SIncr ann i
        RSDecr ann i -> SDecr ann i
        RSRet ann e -> SRet ann (entailExpr e)
        RSVRet ann -> SVRet ann
        RSCond ann c t -> SCond ann (entailExpr c) (entailStmt t)
        RSCondElse ann c t e -> SCondElse ann (entailExpr c) (entailStmt t) (entailStmt e)
        RSWhile ann c b -> SWhile ann (entailExpr c) (entailStmt b)
        RSExp ann e -> SExp ann (entailExpr e)
        RSBlock ann sts -> \k ->
          let composed = (composeSts $ map entailSingle sts) in case k of
            SEmpty _ -> composed
            _        -> SBlock ann composed k
        RSEmpty _ -> id
      composeSts :: [Stmt 'Untyped -> Stmt 'Untyped] -> Stmt 'Untyped
      composeSts fs = foldr (.) id fs $ SEmpty fakeAnn
  in entailSingle s (SEmpty fakeAnn)


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

data FunDef (s :: Stage) = FunDef
  { _fundefAnn :: Ann
  , _fundefRetType :: Type
  , _fundefName :: Id
  , _fundefArgs :: [Arg]
  , _fundefBody :: Stmt s
  }

data ClassDef (s :: Stage) = ClassDef
  { _classdefAnn :: Ann
  , _classdefName :: Id
  , _classdefSuper :: Maybe Id
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
  , _methodName :: Id
  , _methodArgs :: [Arg]
  , _methodBody :: Maybe (Stmt s)
  }

data Field (s :: Stage) = Field
  { _fieldAnn :: Ann
  , _fieldAccess :: ClassMemberAccess
  , _fieldPlace :: ClassMemberPlace
  , _fieldTy :: Type
  , _fieldAssignments :: NonEmpty (Id, Maybe (Expr s))
  }

data Constructor (s :: Stage) = Constructor
  { _constructorAnn :: Ann
  , _constructorAccess :: ClassMemberAccess
  , _constructorName :: Maybe Id
  , _constructorArgs :: [Arg]
  , _constructorBody :: Stmt s
  }

data ClassMember (s :: Stage)
  = CMMethod (Method s)
  | CMField (Field s)
  | CMConstructor (Constructor s)


data ClassMemberPlace = Dynamic | Static
  deriving (Show)
data ClassMemberAccess = Private | Public
  deriving (Show)

newtype Program (s :: Stage) = Program [TopDef s]


makeLensesWith abbreviatedFields ''Ann
makeLensesWith abbreviatedFields ''Arg
makeLensesWith abbreviatedFields ''Type
makeLensesWith abbreviatedFields ''FunDef
makeLensesWith abbreviatedFields ''ClassDef
makeLensesWith abbreviatedFields ''Method
makeLensesWith abbreviatedFields ''Field
makeLensesWith abbreviatedFields ''Constructor


instance Show Id where
  show = iName

instance IsString Id where
  fromString = Id

instance Pretty Id where
  pPrint i = text $ iName i


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


instance Pretty Type where
  pPrint = \case
    TVoid -> "void"
    TInt -> "int"
    TBool -> "bool"
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

block :: Doc -> Doc
block b = lbrace $+$ nest 2 b $+$ rbrace

instance Pretty (Stmt a) where
  pPrint = \case
    SAssg _ v e cont -> pPrint v <+> "=" <+> pPrint e <> semi $+$ pPrint cont
    SFieldAssg _ b v e cont -> pPrint b <> "." <> pPrint v <+> "=" <+> pPrint e <> semi $+$ pPrint cont
    SDecl _ t v cont -> pPrint t <+> pPrint v <> semi $+$ pPrint cont
    SIncr _ v cont -> "++" <> pPrint v <> semi $+$ pPrint cont
    SDecr _ v cont -> "--" <> pPrint v <> semi $+$ pPrint cont
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
    maybe empty pPrint (c^.name) <>
    parens (cat $ punctuate comma $ map pPrint $ c^.args) <+>
    block (pPrint (c^.body))

instance Pretty ClassMemberPlace where
  pPrint = \case
    Dynamic -> empty
    Static -> "static"

instance Pretty ClassMemberAccess where
  pPrint = \case
    Private -> empty
    Public -> "public"

instance Pretty (Program a) where
  pPrint (Program defs) = vcat $ map pPrint defs


pp :: Pretty p => p -> String
pp = render . pPrint

-- instance HasAnn (Expr p) Ann where
--   ann = \case
--     ELit a _ _ -> a
--     EVar a _ _ -> a
--     EApp a _ _ _ -> a
--     EUnOp a _ _ _ -> a
--     EOp  a _ _ _ _ -> a


-- instance HasAnn (Stmt s) Ann where
--   ann = \case
--     SAssg a _ _ _ -> a
--     SDecl a _ _ _ -> a
--     SIncr a _ _ -> a
--     SDecr a _ _ -> a
--     SRet a _ -> a
--     SVRet a -> a
--     SCond a _ _ _ -> a
--     SCondElse a _ _ _ _ -> a
--     SWhile a _ _ _ -> a
--     SExp a _ _ -> a
--     SBlock a _ _ -> a
--     SEmpty -> fakeAnn
