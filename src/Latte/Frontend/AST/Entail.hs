{-# LANGUAGE GADTs #-}
module Latte.Frontend.AST.Entail where

import Latte.Frontend.AST.Types

import Prelude hiding ((<>), EQ, GT, LT)


entailExpr :: RawExpr t -> Expr 'Untyped
entailExpr = \case
  REOr    a e1 e2 -> EOp a () (Op $ Or a)  (entailExpr e1) (entailExpr e2)
  REAnd   a e1 e2 -> EOp a () (Op $ And a) (entailExpr e1) (entailExpr e2)
  RERelOp a o e1 e2 -> EOp a () (Op o) (entailExpr e1) (entailExpr e2)
  REAddOp a o e1 e2 -> EOp a () (Op o) (entailExpr e1) (entailExpr e2)
  REMulOp a o e1 e2 -> EOp a () (Op o) (entailExpr e1) (entailExpr e2)
  RENeg   a e -> EUnOp a () Neg (entailExpr e)
  RENot   a e -> EUnOp a () Not (entailExpr e)
  RELit   a l -> ELit a () l
  REApp   a f as -> EApp a () f (map entailExpr as)
  RENew   a c f as -> ENew a () c f (map entailExpr as)
  REVar   a v -> EVar a () v
  REPar   _ e -> entailExpr e
  RECoe   e -> entailExpr e
  REProj  a e i -> EProj a () (entailExpr e) i
  REMApp  a e i as -> EMApp a () (entailExpr e) i (map entailExpr as)
  RESuper a -> ESuper a ()


entailStmt :: RawStmt -> Stmt 'Untyped
entailStmt s =
  let entailSingle :: RawStmt -> Stmt 'Untyped -> Stmt 'Untyped
      entailSingle = \case
        RSAssg a i v -> SAssg a i (entailExpr v)
        RSFieldAssg a e i v -> SFieldAssg a (entailExpr e) i (entailExpr v)
        RSDecl a t vs ->
          foldr (\(i, mval) lk ->
                   SDecl a t i .
                   case mval of
                     Nothing -> lk
                     Just e -> SAssg a i (entailExpr e) . lk
                ) id vs
        RSIncr a i -> SIncr a i
        RSDecr a i -> SDecr a i
        RSRet a e -> SRet a (entailExpr e)
        RSVRet a -> SVRet a
        RSCond a c t -> SCond a (entailExpr c) (entailStmt t)
        RSCondElse a c t e -> SCondElse a (entailExpr c) (entailStmt t) (entailStmt e)
        RSWhile a c b -> SWhile a (entailExpr c) (entailStmt b)
        RSExp a e -> SExp a (entailExpr e)
        RSBlock a sts -> \k ->
          let composed = (composeSts $ map entailSingle sts) in case k of
            SEmpty _ -> composed
            _        -> SBlock a composed k
        RSEmpty _ -> id
      composeSts :: [Stmt 'Untyped -> Stmt 'Untyped] -> Stmt 'Untyped
      composeSts fs = foldr (.) id fs $ SEmpty fakeAnn
  in entailSingle s (SEmpty fakeAnn)
