{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PatternGuards #-}
module Latte.Backend.X86.PeepHole where

import Latte.Backend.X86.X86
import Prelude as P

optimize :: Assembly -> Assembly
optimize (Assembly is) =
  Assembly
  -- $ jumps . movs
  $ is

jumps :: [Instr] -> [Instr]
jumps = \case
  -- jump to label right after
  Jmp (OLabel l1) : Label l2 : rest
    | l1 == l2 -> jumps (Label l2 : rest)

  -- avoidable jump with condition reverse
  j : Jmp t : Label l3 : rest
    | isCondJump j
    , OLabel l1 <- jumpTarget j
    , l1 == l3
    -> jumps (setJumpTarget t (negJump j) : Label l3 : rest)

  -- nothing to do
  h : t -> h : jumps t
  [] -> []


movs :: [Instr] -> [Instr]
movs = \case
  -- mov to itself
  Mov a a' : rest
    | a == a' -> movs rest

  -- mov and overwrite immediately
  Mov _ b : Mov c b' : rest
    | b == b' && c /= b' -> movs (Mov c b' : rest)

  -- mov from a to b and from b to c where a is in a reg
  Mov a b : Mov b' c : rest
    | b == b' && isReg a && P.not (isReg b) -> movs (Mov a b : Mov a c : rest)

  -- mov from a to b and push b where a is in reg
  Mov a b : Push b' : rest
    | b == b' && isReg a && P.not (isReg b) -> movs (Mov a b : Push a : rest)

  -- mov 0 to reg instead of xor reg reg
  Mov (OConst 0) b : rest
    | isReg b -> movs (Xor b b : rest)

  -- nothing to do
  h : t -> h : movs t
  [] -> []
