{-# LANGUAGE PatternSynonyms #-}
module Latte.Backend.X86.PeepHole where

import Latte.Backend.X86.X86

optimize :: Assembly -> Assembly
optimize (Assembly is) =
  Assembly $ jumps . movs $ is

jumps :: [Instr] -> [Instr]
jumps = \case
  Jmp (OLabel l1) : Label l2 : rest
    | l1 == l2 -> jumps (Label l2 : rest)
  j : Jmp (OLabel l1) : Label l2 : rest
    | isCondJump j && l1 == l2 ->
        jumps (negJump j : Label l2 : rest)
  h : t -> h : jumps t
  [] -> []


movs :: [Instr] -> [Instr]
movs = \case
  Mov a b : rest | a == b -> movs rest
  Mov _ b : Mov c d : rest | b == d && c /= d -> movs (Mov c d : rest)
  Mov a b : Mov c d : rest | b == c && a == d -> movs (Mov a b : rest)
  Mov (OConst 0) b : rest | isReg b -> movs (Xor b b : rest)
  h : t -> h : movs t
  [] -> []
