{-# LANGUAGE OverloadedStrings #-}
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

data Memory = Memory Int Reg (Maybe Reg) Int

data Operand where
  OReg :: Reg -> Operand
  OMem :: Memory -> Operand
  OConst :: Integer -> Operand
  OLabel :: String -> Operand

class IsInteger a where
  fromInteger :: Integer -> a
instance Prelude.Num a => IsInteger a where
  fromInteger = Prelude.fromInteger

data Instr where
  Label :: String -> Instr
  Push :: Operand -> Instr
  Mov :: Operand -> Operand -> Instr

  Add :: Operand -> Operand -> Instr
  Sub :: Operand -> Operand -> Instr
  Imul :: Operand -> Operand -> Instr
  Idiv :: Operand -> Operand -> Instr
  Or :: Operand -> Operand -> Instr
  And :: Operand -> Operand -> Instr
  Xor :: Operand -> Operand -> Instr
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

  Call :: Operand -> Instr
  Leave :: Instr
  Ret :: Instr

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



label :: MonadWriter [Instr] m => String -> m ()
label s = tell [Label s]

push :: MonadWriter [Instr] m => Operand -> m ()
push o = tell [Push o]

mov :: MonadWriter [Instr] m => Operand -> Operand -> m ()
mov l r = tell [Mov l r]

add :: MonadWriter [Instr] m => Operand -> Operand -> m ()
add l r = tell [Add l r]

sub :: MonadWriter [Instr] m => Operand -> Operand -> m ()
sub l r = tell [Sub l r]

imul ::  MonadWriter [Instr] m => Operand -> Operand -> m ()
imul l r = tell [Imul l r]

idiv :: MonadWriter [Instr] m => Operand -> Operand -> m ()
idiv l r = tell [Idiv l r]

or :: MonadWriter [Instr] m => Operand -> Operand -> m ()
or l r = tell [Or l r]

and ::  MonadWriter [Instr] m => Operand -> Operand -> m ()
and l r = tell [And l r]

xor :: MonadWriter [Instr] m => Operand -> Operand -> m ()
xor l r = tell [Xor l r]

cmp ::  MonadWriter [Instr] m => Operand -> Operand -> m ()
cmp l r = tell [Cmp l r]

test ::  MonadWriter [Instr] m => Operand -> Operand -> m ()
test l r = tell [Test l r]


ret :: MonadWriter [Instr] m => m ()
ret = tell [Ret]

leave :: MonadWriter [Instr] m => m ()
leave = tell [Leave]

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
    OLabel l -> pPrint l

instance Pretty Instr where
  pPrint = \case
      Label l -> text l <> ":"
      Push a -> "push" <+> pPrint a
      Mov a b -> "mov" <+> pPrint a <+> pPrint b

      Add a b -> "add" <+> pPrint a <+> pPrint b
      Sub a b -> "sub" <+> pPrint a <+> pPrint b
      Imul a b -> "imul" <+> pPrint a <+> pPrint b
      Idiv a b -> "idiv" <+> pPrint a <+> pPrint b
      Or a b -> "or" <+> pPrint a <+> pPrint b
      And a b -> "and" <+> pPrint a <+> pPrint b
      Xor a b -> "xor" <+> pPrint a <+> pPrint b
      Cmp a b -> "cmp" <+> pPrint a <+> pPrint b
      Test a b -> "test" <+> pPrint a <+> pPrint b

      Jmp a -> "jmp" <+> pPrint a
      Je a -> "je" <+> pPrint a
      Jne a -> "jne" <+> pPrint a
      Jg a -> "jg" <+> pPrint a
      Jge a -> "jge" <+> pPrint a
      Jl a -> "jl" <+> pPrint a
      Jle a -> "jle" <+> pPrint a

      Sete a -> "sete" <+> pPrint a
      Setne a -> "setne" <+> pPrint a
      Setg a -> "setg" <+> pPrint a
      Setge a -> "setge" <+> pPrint a
      Setl a -> "setl" <+> pPrint a
      Setle a -> "setle" <+> pPrint a

      Call a -> "call" <+> pPrint a
      Leave -> "leave"
      Ret -> "ret"

instance Pretty Assembly where
  pPrint (Assembly is) = vcat (map smartPrint is) where
    smartPrint i = case i of
      Label _ -> pPrint i
      _       -> nest 2 (pPrint i)
