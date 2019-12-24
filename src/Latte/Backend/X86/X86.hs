{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Latte.Backend.X86.X86 where

import Control.Monad.Writer

data Reg
  = EAX
  | EBX
  | ECX
  | EDX
  | ESP
  | EBP

data Memory = Memory Int Reg Reg Int

data Operand where
  OReg :: Reg -> Operand
  OMem :: Memory -> Operand
  OConst :: Integer -> Operand
  OLabel :: String -> Operand

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
  Cmp :: Operand -> Operand -> Instr

  Jmp :: Operand -> Instr
  Je :: Operand -> Instr
  Jne :: Operand -> Instr
  Jg :: Operand -> Instr
  Jge :: Operand -> Instr
  Jl :: Operand -> Instr
  Jle :: Operand -> Instr

  Call :: Operand -> Instr
  Leave :: Instr
  Ret :: Instr

label :: MonadWriter [Instr] m => String -> m ()
label s = tell [Label s]

push :: MonadWriter [Instr] m => Operand -> m ()
push o = tell [Push o]

mov :: MonadWriter [Instr] m => Operand -> Operand -> m ()
mov l r = tell [Mov l r]

add :: MonadWriter [Instr] m => Operand -> Operand -> m ()
add l r = tell [Add l r]

ret :: MonadWriter [Instr] m => m ()
ret = tell [Ret]

jmp :: MonadWriter [Instr] m => Operand -> m ()
jmp l = tell [Jmp l]

esp :: Operand
esp = OReg ESP

ebp :: Operand
ebp = OReg EBP

eax :: Operand
eax = OReg EAX

mem :: Int -> (Reg, Reg, Int) -> Operand
mem a (b, c, d) = OMem (Memory a b c d)
