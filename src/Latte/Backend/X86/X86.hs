{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
module Latte.Backend.X86.X86 where

import Data.String
import Text.PrettyPrint.HughesPJClass
import Control.Monad.Writer hiding ((<>))
import Prelude hiding (Num, fromInteger, (<>))
import qualified Prelude

data Reg
  = EAX
  | EBX
  | ECX
  | EDX
  | ESP
  | EBP
  | AL
  | BL
  | CL
  deriving (Eq)

data Memory = Memory Int Reg (Maybe Reg) Int
  deriving (Eq)


data Operand where
  OReg   :: Reg -> Operand
  OMem   :: Memory -> Operand
  OConst :: Integer -> Operand
  OLabel :: String -> Operand
  OVar   :: String -> Operand
  deriving (Eq)

class IsInteger a where
  fromInteger :: Integer -> a
instance Prelude.Num a => IsInteger a where
  fromInteger = Prelude.fromInteger

instance IsString Operand where
  fromString = OLabel

data Instr where
  Ann :: String -> [String] -> Instr
  Comment :: String -> Instr
  Label :: String -> Instr
  Push :: Operand -> Instr
  Pop :: Operand -> Instr
  Mov :: Operand -> Operand -> Instr
  Lea :: Operand -> Operand -> Instr
  Cdq :: Instr

  Add :: Operand -> Operand -> Instr
  Sub :: Operand -> Operand -> Instr
  Imul :: Operand -> Instr
  Idiv :: Operand -> Instr
  Not :: Operand -> Instr
  Or :: Operand -> Operand -> Instr
  And :: Operand -> Operand -> Instr
  Xor :: Operand -> Operand -> Instr
  Neg :: Operand -> Instr
  Cmp :: Operand -> Operand -> Instr
  Test :: Operand -> Operand -> Instr

  Jmp :: Operand -> Instr
  Je :: Operand -> Instr
  Jne :: Operand -> Instr
  Jg :: Operand -> Instr
  Jge :: Operand -> Instr
  Jl :: Operand -> Instr
  Jle :: Operand -> Instr

  Sete :: Operand -> Instr
  Setne :: Operand -> Instr
  Setg :: Operand -> Instr
  Setge :: Operand -> Instr
  Setl :: Operand -> Instr
  Setle :: Operand -> Instr
  Setz :: Operand  -> Instr
  Setnz :: Operand -> Instr

  Call :: Operand -> Instr
  Leave :: Instr
  Ret :: Instr
  deriving (Eq)

type family MemoryBuildF (t :: [*]) = fun | fun -> t where
  MemoryBuildF '[] = Operand
  MemoryBuildF (t ': rest) = t -> MemoryBuildF rest

class MemoryBuild (a :: [*]) where
  mem :: MemoryBuildF a
instance MemoryBuild '[Reg] where
  mem base = OMem (Memory 0 base Nothing 1)
instance MemoryBuild '[Int, Reg] where
  mem off base = OMem (Memory off base Nothing 1)
instance MemoryBuild '[Int, (Reg, Reg, Int)] where
  mem off (base, index, multiplier) = OMem (Memory off base (Just index) multiplier)
instance  MemoryBuild '[(Reg, Reg, Int)] where
  mem (base, index, multiplier) = OMem (Memory 0 base (Just index) multiplier)

data Assembly = Assembly [Instr]

eax :: Operand
eax = OReg EAX

ebx :: Operand
ebx = OReg EBX

ecx :: Operand
ecx = OReg ECX

edx :: Operand
edx = OReg EDX

esp :: Operand
esp = OReg ESP

ebp :: Operand
ebp = OReg EBP

al :: Operand
al = OReg AL

bl :: Operand
bl = OReg BL

cl :: Operand
cl = OReg CL


long :: MonadWriter [Instr] m => String -> m ()
long s = tell [Ann "long" [s]]

string :: MonadWriter [Instr] m => String -> m ()
string s = tell [Ann "string" [show s]]

globl :: MonadWriter [Instr] m => String -> m ()
globl s = tell [Ann "globl" [s]]

comment :: MonadWriter [Instr] m => String -> m ()
comment s = tell [Comment s]

label :: MonadWriter [Instr] m => String -> m ()
label s = tell [Label s]

push :: MonadWriter [Instr] m => Operand -> m ()
push o = tell [Push o]

pop :: MonadWriter [Instr] m => Operand -> m ()
pop o = tell [Pop o]

mov :: MonadWriter [Instr] m => Operand -> Operand -> m ()
mov l r = tell [Mov l r]

lea :: MonadWriter [Instr] m => Operand -> Operand -> m ()
lea l r = tell [Lea l r]

cdq :: MonadWriter [Instr] m => m ()
cdq = tell [Cdq]

add :: MonadWriter [Instr] m => Operand -> Operand -> m ()
add l r = tell [Add l r]

sub :: MonadWriter [Instr] m => Operand -> Operand -> m ()
sub l r = tell [Sub l r]

imul ::  MonadWriter [Instr] m => Operand -> m ()
imul l = tell [Imul l]

idiv :: MonadWriter [Instr] m => Operand -> m ()
idiv l = tell [Idiv l]

neg :: MonadWriter [Instr] m => Operand -> m ()
neg l = tell [Neg l]

or :: MonadWriter [Instr] m => Operand -> Operand -> m ()
or l r = tell [Or l r]

and ::  MonadWriter [Instr] m => Operand -> Operand -> m ()
and l r = tell [And l r]

xor :: MonadWriter [Instr] m => Operand -> Operand -> m ()
xor l r = tell [Xor l r]

not :: MonadWriter [Instr] m => Operand -> m ()
not l = tell [Not l]

cmp ::  MonadWriter [Instr] m => Operand -> Operand -> m ()
cmp l r = tell [Cmp l r]

test ::  MonadWriter [Instr] m => Operand -> Operand -> m ()
test l r = tell [Test l r]


ret :: MonadWriter [Instr] m => m ()
ret = tell [Ret]

leave :: MonadWriter [Instr] m => m ()
leave = tell [Leave]

call :: MonadWriter [Instr] m => Operand -> m ()
call f = tell [Call f]


jmp :: MonadWriter [Instr] m => Operand -> m ()
jmp l = tell [Jmp l]


je :: MonadWriter [Instr] m => Operand -> m ()
je l = tell [Je l]

jne ::  MonadWriter [Instr] m => Operand -> m ()
jne l = tell [Jne l]

jg :: MonadWriter [Instr] m => Operand -> m ()
jg l = tell [Jg l]

jge :: MonadWriter [Instr] m => Operand -> m ()
jge l = tell [Jge l]

jl :: MonadWriter [Instr] m => Operand -> m ()
jl l = tell [Jl l]

jle :: MonadWriter [Instr] m => Operand -> m ()
jle l = tell [Jle l]


sete :: MonadWriter [Instr] m => Operand -> m ()
sete l = tell [Sete l]

setne ::  MonadWriter [Instr] m => Operand -> m ()
setne l = tell [Setne l]

setg :: MonadWriter [Instr] m => Operand -> m ()
setg l = tell [Setg l]

setge :: MonadWriter [Instr] m => Operand -> m ()
setge l = tell [Setge l]

setl :: MonadWriter [Instr] m => Operand -> m ()
setl l = tell [Setl l]

setle :: MonadWriter [Instr] m => Operand -> m ()
setle l = tell [Setle l]

setz :: MonadWriter [Instr] m => Operand -> m ()
setz l = tell [Setz l]

setnz :: MonadWriter [Instr] m => Operand -> m ()
setnz l = tell [Setnz l]


negJump :: Instr -> Instr
negJump = \case
  Je a  -> Jne a
  Jne a -> Je a
  Jg a  -> Jle a
  Jge a -> Jl a
  Jl a  -> Jge a
  Jle a -> Jg a
  e     -> error $ "Not a jump instr: " ++ render (pPrint e)

isCondJump :: Instr -> Bool
isCondJump = \case
  Je _  -> True
  Jne _ -> True
  Jg _  -> True
  Jge _ -> True
  Jl _  -> True
  Jle _ -> True
  _     -> False

jumpTarget :: Instr -> Operand
jumpTarget = \case
  Je a  -> a
  Jne a -> a
  Jg a  -> a
  Jge a -> a
  Jl a  -> a
  Jle a -> a
  Jmp a -> a
  e     -> error $ "Not a jump instr: " ++ render (pPrint e)

setJumpTarget :: Operand -> Instr -> Instr
setJumpTarget t = \case
  Je _  -> Je t
  Jne _ -> Jne t
  Jg _  -> Jg t
  Jge _ -> Jge t
  Jl _  -> Jl t
  Jle _ -> Jle t
  Jmp _ -> Jmp t
  e     -> error $ "Not a jump instr: " ++ render (pPrint e)


isMem :: Operand -> Bool
isMem = \case
  OMem _ -> True
  _ -> False

isVar :: Operand -> Bool
isVar = \case
  OMem _ -> True
  OReg _ -> True
  _ -> False

isReg :: Operand -> Bool
isReg = \case
  OReg _ -> True
  _ -> False

instance Pretty Reg where
  pPrint = \case
    EAX -> "%eax"
    EBX -> "%ebx"
    ECX -> "%ecx"
    EDX -> "%edx"
    ESP -> "%esp"
    EBP -> "%ebp"
    AL  -> "%al"
    BL  -> "%bl"
    CL  -> "%cl"

instance Pretty Memory where
  pPrint = \case
    Memory 0 r Nothing 1 -> parens (pPrint r)
    Memory o r Nothing 1 -> pPrint o <> parens (pPrint r)
    Memory 0 r (Just r2) 1 -> parens (comma <> pPrint r <> comma <> pPrint r2)
    Memory o b (Just i) m -> pPrint o <> parens (pPrint b <> comma <> pPrint i <> comma <> pPrint m)
    Memory o b Nothing m -> pPrint o <> parens (pPrint b <> comma <> comma <> pPrint m)

instance Pretty Operand where
  pPrint = \case
    OMem m -> pPrint m
    OReg r -> pPrint r
    OConst c -> "$" <> pPrint c
    OLabel l -> text l
    OVar v -> text v

instance Pretty Instr where
  pPrint = \case
    Ann a as -> "." <> text a <+> vcat (punctuate space (map text as))
    Comment s -> "#" <+> text s
    Label l -> text l <> ":"
    Push a -> "pushl" <+> pPrint a
    Pop a -> "popl" <+> pPrint a
    Mov a b -> "movl" <+> pPrint a <> comma <+> pPrint b
    Lea a b -> "lea" <+> pPrint a <> comma <+> pPrint b
    Cdq -> "cdq"

    Add a b -> "add" <+> pPrint a <> comma <+> pPrint b
    Sub a b -> "sub" <+> pPrint a <> comma <+> pPrint b
    Imul a -> "imul" <+> pPrint a
    Idiv a -> "idiv" <+> pPrint a
    Neg a -> "neg" <+> pPrint a
    Or a b -> "or" <+> pPrint a <> comma <+> pPrint b
    And a b -> "and" <+> pPrint a <> comma <+> pPrint b
    Xor a b -> "xor" <+> pPrint a <> comma <+> pPrint b
    Not a -> "not" <+> pPrint a
    Cmp a b -> "cmp" <+> pPrint a <> comma <+> pPrint b
    Test a b -> "test" <+> pPrint a <> comma <+> pPrint b

    Jmp a@(OLabel _) -> "jmp" <+> pPrint a
    Je a@(OLabel _) -> "je" <+> pPrint a
    Jne a@(OLabel _) -> "jne" <+> pPrint a
    Jg a@(OLabel _) -> "jg" <+> pPrint a
    Jge a@(OLabel _) -> "jge" <+> pPrint a
    Jl a@(OLabel _) -> "jl" <+> pPrint a
    Jle a@(OLabel _) -> "jle" <+> pPrint a

    Jmp a -> "jmp" <+> "*" <> pPrint a
    Je a -> "je" <+> "*" <>  pPrint a
    Jne a -> "jne" <+> "*" <>  pPrint a
    Jg a -> "jg" <+> "*" <>  pPrint a
    Jge a -> "jge" <+> "*" <>  pPrint a
    Jl a -> "jl" <+> "*" <>  pPrint a
    Jle a -> "jle" <+> "*" <>  pPrint a

    Sete a -> "sete" <+> pPrint a
    Setne a -> "setne" <+> pPrint a
    Setg a -> "setg" <+> pPrint a
    Setge a -> "setge" <+> pPrint a
    Setl a -> "setl" <+> pPrint a
    Setle a -> "setle" <+> pPrint a
    Setz a -> "setz" <+> pPrint a
    Setnz a -> "setnz" <+> pPrint a

    Call a@(OLabel _) -> "call" <+> pPrint a
    Call a -> "call" <+> "*" <> pPrint a
    Leave -> "leave"
    Ret -> "ret"

instance Pretty Assembly where
  pPrint (Assembly is) = vcat (map smartPrint is) where
    smartPrint i = case i of
      Label _   -> pPrint i
      -- Ann _ _ -> pPrint i
      Comment _ -> nest 2 (pPrint i)
      _         -> nest 4 (pPrint i)
