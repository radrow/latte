{-# LANGUAGE RankNTypes #-}
module Latte.PP where

import Control.Monad.Free
import Data.List.NonEmpty(toList)
import Latte.Types.Free
import Control.Monad.State

import qualified Latte.Types.Syntax as S(Op)
import Latte.Types.Syntax as S hiding (Op)

indentFactor :: Int
indentFactor = 2


indent :: Int -> String -> String
indent i s = take (i * indentFactor) (repeat ' ') ++ s

ppComma :: (a -> String) -> [a] -> String
ppComma _ [] = ""
ppComma p [arg] = p arg
ppComma p (a:rest) = p a ++ ", " ++ ppComma p rest

ppArg :: Arg -> String
ppArg (Arg _ t i) = ppType t ++ " " ++ iName i

ppType :: Type -> String
ppType = \case
  TInt _ -> "int"
  TString _ -> "string"
  TBool _ -> "bool"
  TVoid _ -> "void"
  TFun _ targs rett -> ppType rett ++ "(" ++ ppComma ppType targs ++ ")"

ppOp :: Op -> String
ppOp = \case
  Op (S.LT _)    -> "<"
  Op (S.LEQ _)   -> "<="
  Op (S.EQ _)    -> "=="
  Op (S.NEQ _)   -> "!="
  Op (S.GEQ _)   -> ">="
  Op (S.GT _)    -> ">"
  Op (S.Plus _)  -> "+"
  Op (S.Minus _) -> "-"
  Op (S.Mult _)  -> "*"
  Op (S.Div _)   -> "/"
  Op (S.Mod _)   -> "%"
  Op (S.Or _)    -> "||"
  Op (S.And _)   -> "&&"


-- ppFunDef :: FunDef -> String
-- ppFunDef (FunDef _ t name args body) =
--   ppType t ++ " " ++ iName name ++ "(" ++ ppComma ppArg args ++ ")\n" ++ ppStmtI 1 body

-- ppProgram :: Program -> String
-- ppProgram (Program funs) =
--   concatMap (\f -> ppFunDef f ++ "\n\n") funs

type Printer a = String -> a

ind :: ((Int -> String) -> a) -> String -> Printer a
ind k x = \s -> k (\i -> indent i x ++ s)

ppExpr :: ExprM (Int -> String) a -> Printer a
ppExpr act = foldFree interpret act where
  interpret :: forall x. ExprF (Int -> String) x -> Printer x
  interpret = \case
    EVarF _ x k -> ind k (iName x)
    EAppF _ f args k -> ind k $ iName f ++ "(" ++ ppComma id (map ($0) args) ++ ")"
    ELitF _ l k -> ind k $ case l of
                             LInt i -> show i
                             LBool True -> "true"
                             LBool False -> "false"
                             LString s -> show s
    ENegF _ e k -> ind k ("-" ++ e 0)
    ENotF _ e k -> ind k ("!" ++ e 0)
    EOpF  _ op l r k -> ind k ("(" ++ l 0 ++ " " ++ ppOp op ++ " " ++ r 0 ++ ")")

ppStmt :: StmtM (Int -> String) (Int -> String) a -> Printer a
ppStmt act = foldFree interpret act where
  interpret :: forall x. StmtF (Int -> String) (Int -> String) x -> Printer x
  interpret = \case
    SAssgF _ n v k -> ind k $ iName n ++ " = " ++ v 0 ++ ";"
    SDeclF _ t vs k -> ind k $ ppType t ++ " " ++ ppComma dec (toList vs) ++ ";" where
      dec (n, Nothing) = iName n
      dec (n, Just e) = iName n ++ " = " ++ e 0 ++ ";"
    SIncrF _ v k -> ind k $ iName v ++ "++" ++ ";"
    SDecrF _ v k -> ind k $ iName v ++ "--" ++ ";"
    SRetF _ e k -> ind k $ "return " ++ e 0 ++ ";"
    SVRetF _ k -> ind k $ "return;"
    SCondF _ c t k -> \s -> k $ \i ->
      indent i ("if(" ++ c 0 ++ ")\n") ++ t (i + 1) ++ s
    SCondElseF _ c t e k -> \s -> k $ \i ->
      indent i ("if(" ++ c 0 ++ ")\n") ++ t (i + 1) ++ "\n" ++
      indent i "else\n" ++ e (i + 1) ++ s
    SWhileF _ c b k -> \s -> k $ \i ->
      indent i ("if(" ++ c 0 ++ ")\n") ++ b (i + 1) ++ s
    SExpF _ e k -> \s -> k $ \i -> e i ++ ";" ++ s
    SBlockF _ [] k -> ind k "{}"
    SBlockF _ sts k ->
      \s -> k $ \i -> indent i "{\n" ++ concatMap (\f -> f (i + 1) ++ "\n") sts ++ indent i "}" ++ s
    SEmptyF _ k -> ind k ""
    SLiftExprF ex k -> k <$> ppExpr ex
