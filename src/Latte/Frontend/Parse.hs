{-# LANGUAGE OverloadedStrings #-}
module Latte.Frontend.Parse where

import           Data.Functor(void, ($>))
import           Control.Applicative        (liftA2)
import           Control.Applicative.Combinators.NonEmpty(sepBy1)
import           Control.Monad
import           Control.Monad.Identity
import           Data.Void
import           Data.Text(Text)
import           Data.List as DL
import           Data.Bifunctor
import           Text.Megaparsec hiding (sepBy1)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Prelude hiding (lex, LT, GT, EQ)

import qualified Latte.Frontend.AST as AST


type Parser = ParsecT Void Text Identity
runLatteParser :: Parser a -> FilePath -> Text -> Either String a
runLatteParser p filename inp = first
  (concat . fmap parseErrorPretty . bundleErrors)
  (parse (skip *> p <* eof) filename inp)


getAnn :: Parser AST.Ann
getAnn = do
  p <- getSourcePos
  return $ AST.Ann
    (sourceName p)
    (unPos $ sourceLine p)
    (unPos $ sourceColumn p)


withAnn :: Parser (AST.Ann -> a) -> Parser a
withAnn p = liftA2 (flip ($)) getAnn p


withAnnP :: (AST.Ann -> a) -> Parser a
withAnnP = withAnn . pure
{-# INLINE withAnnP #-}


keywords :: [String]
keywords =
  [ "return"
  , "while"
  , "if"
  , "else"
  , "int", "string", "bool", "void"
  , "true", "false"
  , "class"
  ]


skip :: Parser ()
skip = L.space
  (void spaceChar)
  (L.skipLineComment "//" <|> L.skipLineComment "#")
  (L.skipBlockComment "/*" "*/")


lex :: Parser a -> Parser a
lex = L.lexeme skip


lId :: Parser String
lId = lex $ liftA2 (:) lowerChar (many alphaNumChar)


signed :: Parser Integer
signed = lex $ L.decimal


operator :: Text -> Parser ()
operator o =
  lex $ try $ string o *> notFollowedBy (oneOf ("=+-/*%\\&.|^<>" :: String))


symbol :: Text -> Parser Text
symbol = L.symbol skip


word :: Text -> Parser ()
word w = lex $ try $ string w >> notFollowedBy alphaNumChar >> skip


paren :: Parser a -> Parser a
paren = between (L.symbol skip "(") (L.symbol skip ")")


brac :: Parser a -> Parser a
brac = between (L.symbol skip "{") (L.symbol skip "}")


escapedChar :: Parser Char
escapedChar = do
  c <- try (printChar >>= \cc -> when (cc == '"') mzero >> pure cc)
  if c /= '\\'
    then pure c
    else printChar >>= \case
    'n'  -> pure '\n'
    't'  -> pure '\t'
    '\\' -> pure '\\'
    'r'  -> pure '\r'
    'v'  -> pure '\v'
    'b'  -> pure '\b'
    'f'  -> pure '\f'
    '0'  -> pure '\0'
    bad  -> fail $ "Cannot escape char '" <> [bad] <> "'"


infixL :: Parser (a -> b -> a) -> Parser b -> a -> Parser a
infixL op p x = do
  f <- op
  y <- p
  let r = f x y
  infixL op p r <|> return r


ident :: Parser AST.Id
ident =  lId >>= \i -> if (i `elem` keywords)
  then fail $ "Forbidden id: " ++ i else pure (AST.Id i)


lit :: Parser AST.Lit
lit = choice
  [ AST.LInt <$> signed
  , AST.LString <$> between (symbol "\"") (symbol "\"") (many escapedChar)
  , AST.LBool <$> ((True <$ word "true") <|> (False <$ word "false"))
  ]


relOp :: Parser (AST.Op 'AST.Rel)
relOp = choice
  [ withAnn $ AST.LT  <$ operator "<"
  , withAnn $ AST.LEQ <$ operator "<="
  , withAnn $ AST.EQ  <$ operator "=="
  , withAnn $ AST.GEQ <$ operator ">="
  , withAnn $ AST.GT  <$ operator ">"
  , withAnn $ AST.NEQ <$ operator "!="
  ]

addOp :: Parser (AST.Op 'AST.Add)
addOp = choice
  [ withAnn $ AST.Plus  <$ operator "+"
  , withAnn $ AST.Minus <$ operator "-"
  ]

mulOp :: Parser (AST.Op 'AST.Mul)
mulOp = choice
  [ withAnn $ AST.Mult <$ operator "*"
  , withAnn $ AST.Div  <$ operator "/"
  , withAnn $ AST.Mod  <$ operator "%"
  ]

expr :: Parser (AST.Expr 'AST.Untyped)
expr = AST.entailExpr <$> expr0

rawExpr :: Parser (AST.RawExpr 0)
rawExpr = expr0

expr0 :: Parser (AST.RawExpr 0)
expr0 = do
  e <- expr1
  choice [ withAnnP AST.REOr <*> (operator "||" $> e) <*> expr0
         , pure $ AST.RECoe e
         ]


expr1 :: Parser (AST.RawExpr 1)
expr1 = do
  e <- expr2
  choice [ withAnnP AST.REAnd <*> (operator "&&" $> e) <*> expr1
         , pure $ AST.RECoe e
         ]


expr2 :: Parser (AST.RawExpr 2)
expr2 = do
  e <- AST.RECoe <$> expr3
  choice [ try $ (pure e) >>= infixL (withAnn $ flip AST.RERelOp <$> relOp) expr3
         , pure e
         ]


expr3 :: Parser (AST.RawExpr 3)
expr3 = do
  e <- AST.RECoe <$> expr4
  choice [ try $ (pure e) >>= infixL (withAnn $ flip AST.REAddOp <$> addOp) expr4
         , pure e
         ]


expr4 :: Parser (AST.RawExpr 4)
expr4 = do
  e <- AST.RECoe <$> expr5
  choice [ try $ (pure e) >>= infixL (withAnn $ flip AST.REMulOp <$> mulOp) expr5
         , pure e
         ]


expr5 :: Parser (AST.RawExpr 5)
expr5 = choice
  [ operator "!" *> withAnnP AST.RENot <*> expr6
  , operator "-" *> withAnnP AST.RENeg <*> expr6
  , AST.RECoe <$> expr6
  ]


expr6 :: Parser (AST.RawExpr 6)
expr6 = do
  e <- AST.RECoe <$> expr7
  choice [ try $ (pure e) >>= infixL (withAnn $ operator "." $> AST.REProj) ident
         , pure e
         ]


expr7 :: Parser (AST.RawExpr 7)
expr7 = choice
  [ withAnnP AST.REPar <*> paren expr0
  , withAnnP AST.RELit <*> lit
  , try $ withAnnP AST.REApp <*> ident <*> paren (sepBy expr0 (symbol ","))
  , withAnnP AST.REVar <*> ident
  ]


rawDecl :: Parser (AST.Id, Maybe (AST.RawExpr 0))
rawDecl = liftA2 (,) (try $ ident <* operator "=") (Just <$> rawExpr)
  <|> liftA2 (,) ident (pure Nothing)


decl :: Parser (AST.Id, Maybe (AST.Expr 'AST.Untyped))
decl = fmap (fmap AST.entailExpr) <$> rawDecl


semicolon :: Parser ()
semicolon = void $ symbol ";"


stmt :: Parser AST.RawStmt
stmt = choice
  [ block
  , withAnnP AST.RSAssg <*> try (ident <* operator "=") <*> rawExpr <* semicolon
  , withAnnP AST.RSDecl <*> type_ <*> sepBy1 rawDecl (symbol ",") <* semicolon
  , withAnnP AST.RSIncr <*> try (ident <* operator "++") <* semicolon
  , withAnnP AST.RSDecr <*> try (ident <* operator "--") <* semicolon
  , withAnnP AST.RSRet <*> (try $ word "return" *> rawExpr) <* semicolon
  , withAnn (AST.RSVRet <$ word "return") <* semicolon
  , withAnnP (\a c t me -> case me of {Nothing -> AST.RSCond a c t; Just e -> AST.RSCondElse a c t e})
    <*> (word "if" *> paren rawExpr) <*> stmt <*> optional (word "else" *> stmt)
  , withAnnP AST.RSWhile <*> (word "while" *> paren rawExpr) <*> stmt
  , withAnnP AST.RSExp <*> rawExpr <* semicolon
  , withAnn (pure AST.RSEmpty) <* semicolon
  ]


block :: Parser AST.RawStmt
block = withAnnP AST.RSBlock <*> brac (many stmt)


body :: Parser (AST.Stmt 'AST.Untyped)
body = AST.entailStmt <$> block


type_ :: Parser AST.Type
type_ = choice
  [ AST.TInt <$ word "int"
  , AST.TBool <$ word "bool"
  , AST.TString <$ word "string"
  , AST.TVoid <$ word "void"
  ]


arg :: Parser AST.Arg
arg = withAnnP AST.Arg <*> type_ <*> ident


topDef :: Parser (AST.TopDef 'AST.Untyped)
topDef = choice
  [ AST.TDFun <$> funDef
  , AST.TDClass <$> classDef
  ]

classDef :: Parser (AST.ClassDef 'AST.Untyped)
classDef = withAnnP AST.ClassDef <*> try (word "class" *> ident) <*> (optional $ word "extends" *> ident) <*> brac (many classMember)

funDef :: Parser (AST.FunDef 'AST.Untyped)
funDef = withAnnP AST.FunDef <*> type_ <*> ident <*> paren (sepBy arg (symbol ",")) <*> body


classMember :: Parser (AST.ClassMember 'AST.Untyped)
classMember = error "TODO"
-- classMember = choice
--   [-- withAnnP AbstractMethod <*> (word "abstract" *> classMemberAccess) <*> classMemberPlace
--    -- <*> type_ <*> ident <*> paren (sepBy arg (symbol ","))
--   -- , withAnnP Method <*> classMemberAccess <*> classMemberPlace
--   --   <*> type_ <*> ident <*> paren (sepBy arg (symbol ",")) <*> stmt
--   -- , withAnnP Constructor <*> (word "constructor" *> classMemberAccess)
--   --   <*> option ident <*> paren (sepBy arg (symbol ",")) stmt
--    withAnnP AST.Field <*> classMemberAccess <*> classMemberPlace <*> type_
--     <*> sepBy1 decl (symbol ",") <* semicolon
--   ]


classMemberAccess :: Parser AST.ClassMemberAccess
classMemberAccess =
  word "public" $> AST.Public <|> pure AST.Private


classMemberPlace :: Parser AST.ClassMemberPlace
classMemberPlace =
  word "static" $> AST.Static <|> pure AST.Dynamic

program :: Parser (AST.Program 'AST.Untyped)
program = AST.Program <$> many topDef