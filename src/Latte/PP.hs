module Latte.PP where


import Data.List.NonEmpty(toList)
import Latte.Types.AST
import Latte.Types.Latte
import qualified Latte.Types.Syntax as S

indentFactor :: Int
indentFactor = 2


indent :: Int -> String -> String
indent i s = take (i * indentFactor) (repeat ' ') ++ s


ppExpr :: Expr a -> String
ppExpr = \case
  ELit _ _ l -> case l of
                LInt i -> show i
                LBool True -> "true"
                LBool False -> "false"
                LString s -> show s
  EApp _ _ f args -> iName f ++ "(" ++ ppComma ppExpr args ++ ")"
  EVar _ _ x -> iName x
  ENeg _ _ e -> "-" ++ ppExpr e
  ENot _ _ e -> "!" ++ ppExpr e
  EOp _ _ op a b -> "(" ++ ppExpr a ++ " " ++ ppOp op ++ " " ++ ppExpr b ++ ")"

ppComma :: (a -> String) -> [a] -> String
ppComma _ [] = ""
ppComma p [arg] = p arg
ppComma p (a:rest) = p a ++ ", " ++ ppComma p rest

ppArg :: Arg -> String
ppArg (Arg _ t i) = ppType t ++ " " ++ iName i

ppType :: Type -> String
ppType = \case
  TInt -> "int"
  TString -> "string"
  TBool -> "bool"
  TVoid -> "void"

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

ppStmt :: Stmt (Expr a) -> String
ppStmt s = ppStmtI 0 s

ppStmtI :: Int -> Stmt (Expr a) -> String
ppStmtI i s = indent i st where
  st = case s of
    SAssg _ n v k -> iName n <> " = " <> ppExpr v <> goOn k
    SDecl _ t vs k -> ppType t <> " " <> ppComma dec (toList vs) <> goOn k where
      dec (n, Nothing) = iName n
      dec (n, Just e) = iName n <> " = " <> ppExpr e
    SIncr _ v k -> iName v <> "++" <> goOn k
    SDecr _ v k -> iName v <> "--" <> goOn k
    SRet _ e -> "return " <> ppExpr e
    SVRet _ -> "return"
    SCond _ c t k -> "if(" <> ppExpr c <> ")\n" <> ppStmtI (i + 1) t  <> goOn k
    SCondElse _ c t e k -> "if(" <> ppExpr c <> ")\n" <> ppStmtI (i + 1) t <> "\n" <>
      indent i "else\n" <> ppStmtI (i + 1) e  <> goOn k
    SWhile _ c b k -> "while(" <> ppExpr c <> ")\n" <> ppStmtI (i + 1) b <> goOn k
    SExp _ e k -> ppExpr e <> goOn k
    SBlock _ b k -> "{\n" <> ppStmtI (i+1) b <> indent i "}" <> goOn k
    SEmpty -> ""
  goOn k = "\n" <> ppStmtI i k


ppTopDef :: TopDef a -> String
ppTopDef (FunDef _ t name args body) =
  ppType t ++ " " ++ iName name ++ "(" ++ ppComma ppArg args ++ ")\n" ++ ppStmtI 1 body

ppProgram :: Program a -> String
ppProgram (Program funs) =
  concatMap (\f -> ppTopDef f ++ "\n\n") funs
