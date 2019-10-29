module Latte.PP where


import Data.List.NonEmpty(toList)
import Latte.Types.AST
import Latte.Types.Syntax(Lit(..), Arg(..), Type(..), iName, Stmt(..))
import qualified Latte.Types.Syntax as S

indentFactor :: Int
indentFactor = 2


indent :: Int -> String -> String
indent i s = take (i * indentFactor) (repeat ' ') ++ s


ppExpr :: Expr -> String
ppExpr = \case
  ELit _ l -> case l of
                LInt i -> show i
                LBool True -> "true"
                LBool False -> "false"
                LString s -> show s
  EApp _ f args -> iName f ++ "(" ++ ppComma ppExpr args ++ ")"
  EVar _ x -> iName x
  ENeg _ e -> "-" ++ ppExpr e
  ENot _ e -> "!" ++ ppExpr e
  EOp _ op a b -> "(" ++ ppExpr a ++ " " ++ ppOp op ++ " " ++ ppExpr b ++ ")"

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

ppStmt :: Stmt Expr -> String
ppStmt s = ppStmtI 0 s

ppStmtI :: Int -> Stmt Expr -> String
ppStmtI i s = indent i st where
  st = case s of
    SAssg _ n v -> iName n ++ " = " ++ ppExpr v
    SDecl _ t vs -> ppType t ++ " " ++ ppComma dec (toList vs) where
      dec (n, Nothing) = iName n
      dec (n, Just e) = iName n ++ " = " ++ ppExpr e
    SIncr _ v -> iName v ++ "++"
    SDecr _ v -> iName v ++ "--"
    SRet _ e -> "return " ++ ppExpr e
    SVRet _ -> "return"
    SCond _ c t -> "if(" ++ ppExpr c ++ ")\n" ++ ppStmtI (i + 1) t
    SCondElse _ c t e -> "if(" ++ ppExpr c ++ ")\n" ++ ppStmtI (i + 1) t ++ "\n" ++
      indent i "else\n" ++ ppStmtI (i + 1) e
    SWhile _ c b -> "while(" ++ ppExpr c ++ ")\n" ++ ppStmtI (i + 1) b
    SExp _ e -> ppExpr e
    SBlock _ [] -> "{}"
    SBlock _ sts -> "{\n" ++
      concatMap ((++";\n") . ppStmtI (i + 1)) sts ++
      indent i "}"
    SEmpty _ -> ""


ppFunDef :: FunDef -> String
ppFunDef (FunDef _ t name args body) =
  ppType t ++ " " ++ iName name ++ "(" ++ ppComma ppArg args ++ ")\n" ++ ppStmtI 1 body

ppProgram :: Program -> String
ppProgram (Program funs) =
  concatMap (\f -> ppFunDef f ++ "\n\n") funs
