{-# LANGUAGE StandaloneDeriving #-}
module Latte.Types.Syntax
  ( Op(..), OpType(..), RawExpr(..), RawStmt(..)
  ) where

import Data.List.NonEmpty(NonEmpty)
import GHC.TypeNats(Nat, type (+))
import Latte.Types.Latte( Id, Ann, Lit, Type, Arg, HasAnn(getAnn)
                        , Stage(..)
                        )


data OpType = Rel | Add | Mul | Log
  deriving (Show)


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
deriving instance Show (Op t)


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
  REVar   :: Ann -> Id                                -> RawExpr 7
  REPar   :: Ann -> RawExpr 0                         -> RawExpr 7
  RECoe   ::        RawExpr (n + 1)                   -> RawExpr n
deriving instance Show (RawExpr n)


type E = RawExpr 0


data RawStmt
  = RSAssg Ann Id E
  | RSDecl Ann (Type 'Untyped) (NonEmpty (Id, Maybe E))
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
  deriving (Show)

