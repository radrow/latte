{-# LANGUAGE RankNTypes #-}
module Latte.PP where

import Control.Monad.Free
import Data.List.NonEmpty(toList)
import Latte.Types.Free

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


type IndentCont = Int -> String
type Printer a = String -> a


ind :: (IndentCont -> a) -> String -> Printer a
ind k x = \s -> k (\i -> indent i x ++ s)


ppExpr :: ExprM String a -> Printer a
ppExpr = foldFree $ \case
  EVarF _ x k -> \s -> k $ iName x ++ s
  EAppF _ f args k -> \s -> k $ iName f ++ "(" ++ ppComma id args ++ ")" ++ s
  ELitF _ l k -> \s -> k $ case l of
                             LInt i -> show i ++ s
                             LBool True -> "true" ++ s
                             LBool False -> "false" ++ s
                             LString s -> show s ++ s
  ENegF _ e k -> \s -> k ("-" ++ e)
  ENotF _ e k -> \s -> k ("!" ++ e)
  EOpF  _ op l r k -> \s -> k ("(" ++ l ++ " " ++ ppOp op ++ " " ++ r ++ ")")


ppStmt :: StmtM String IndentCont a -> Printer a
ppStmt = foldFree interpret where
  interpret :: forall x. StmtF String IndentCont x -> Printer x
  interpret = \case
    SAssgF _ n v k -> ind k $ iName n ++ " = " ++ v ++ ";"
    SDeclF _ t vs k -> ind k $ ppType t ++ " " ++ ppComma dec (toList vs) ++ ";" where
      dec (n, Nothing) = iName n
      dec (n, Just e) = iName n ++ " = " ++ e ++ ";"
    SIncrF _ v k -> ind k $ iName v ++ "++" ++ ";"
    SDecrF _ v k -> ind k $ iName v ++ "--" ++ ";"
    SRetF _ e k -> ind k $ "return " ++ e ++ ";"
    SVRetF _ k -> ind k $ "return;"
    SCondF _ c t k -> \s -> k $ \i ->
      indent i ("if(" ++ c ++ ")\n") ++ t (i + 1) ++ s
    SCondElseF _ c t e k -> \s -> k $ \i ->
      indent i ("if(" ++ c ++ ")\n") ++ t (i + 1) ++ "\n" ++
      indent i "else\n" ++ e (i + 1) ++ s
    SWhileF _ c b k -> \s -> k $ \i ->
      indent i ("if(" ++ c ++ ")\n") ++ b (i + 1) ++ s
    SExpF _ e k -> ind k (e ++ ";")
    SBlockF _ [] k -> ind k "{}"
    SBlockF _ sts k ->
      \s -> k $ \i -> indent i "{\n" ++ concatMap (\f -> f (i + 1) ++ "\n") sts ++ indent i "}" ++ s
    SEmptyF _ k -> ind k ""
    SLiftExprF ex k -> k <$> ppExpr ex


ppTopDef :: TopDefM String IndentCont a -> Printer a
ppTopDef = foldFree interpret where
  interpret :: TopDefF String IndentCont a -> Printer a
  interpret = \case
    FunDefF _ t n args stmts k -> \s -> k $ \_ ->
      ppType t ++ " " ++ iName n ++ "(" ++ ppComma ppArg args ++ ") " ++ stmts 0 ++ s
    LiftStmtF ex k -> k <$> ppStmt ex


ppProgram :: ProgramM String IndentCont a -> Printer a
ppProgram = foldFree interpret where
  interpret :: ProgramF String IndentCont a -> Printer a
  interpret = \case
    ProgramF tops k -> \s -> k $ \_ -> concatMap ((++"\n\n"). ($0)) tops ++ s
    LiftTopDefF ex k -> k <$> ppTopDef ex


prettyPrint :: ProgramM String IndentCont IndentCont -> String
prettyPrint p = ppProgram p "" 0
