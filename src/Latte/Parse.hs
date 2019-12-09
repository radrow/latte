{-# LANGUAGE OverloadedStrings #-}
module Latte.Parse where

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

import Latte.Types.Syntax
import Latte.Types.AST
import Latte.Types.Latte hiding (getAnn)


type Parser = ParsecT Void Text Identity
runLatteParser :: Parser a -> FilePath -> Text -> Either String a
runLatteParser p filename inp = first
  (concat . fmap parseErrorPretty . bundleErrors)
  (parse (skip *> p <* eof) filename inp)


getAnn :: Parser Ann
getAnn = do
  p <- getSourcePos
  return $ Ann
    (sourceName p)
    (unPos $ sourceLine p)
    (unPos $ sourceColumn p)


withAnn :: Parser (Ann -> a) -> Parser a
withAnn p = liftA2 (flip ($)) getAnn p


withAnnP :: (Ann -> a) -> Parser a
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


ident :: Parser Id
ident =  lId >>= \i -> if (i `elem` keywords)
  then fail $ "Forbidden id: " ++ i else pure (Id i)


lit :: Parser Lit
lit = choice
  [ LInt <$> signed
  , LString <$> between (symbol "\"") (symbol "\"") (many escapedChar)
  , LBool <$> ((True <$ word "true") <|> (False <$ word "false"))
  ]


relOp :: Parser (Op 'Rel)
relOp = choice
  [ withAnn $ LT  <$ operator "<"
  , withAnn $ LEQ <$ operator "<="
  , withAnn $ EQ  <$ operator "=="
  , withAnn $ GEQ <$ operator ">="
  , withAnn $ GT  <$ operator ">"
  , withAnn $ NEQ <$ operator "!="
  ]

addOp :: Parser (Op 'Add)
addOp = choice
  [ withAnn $ Plus  <$ operator "+"
  , withAnn $ Minus <$ operator "-"
  ]

mulOp :: Parser (Op 'Mul)
mulOp = choice
  [ withAnn $ Mult <$ operator "*"
  , withAnn $ Div  <$ operator "/"
  , withAnn $ Mod  <$ operator "%"
  ]

expr :: Parser (Expr 'Untyped)
expr = entailExpr <$> expr0

rawExpr :: Parser (RawExpr 0)
rawExpr = expr0

expr0 :: Parser (RawExpr 0)
expr0 = do
  e <- expr1
  choice [ withAnnP REOr <*> (operator "||" $> e) <*> expr0
         , pure $ RECoe e
         ]


expr1 :: Parser (RawExpr 1)
expr1 = do
  e <- expr2
  choice [ withAnnP REAnd <*> (operator "&&" $> e) <*> expr1
         , pure $ RECoe e
         ]


expr2 :: Parser (RawExpr 2)
expr2 = do
  e <- RECoe <$> expr3
  choice [ try $ (pure e) >>= infixL (withAnn $ flip RERelOp <$> relOp) expr3
         , pure e
         ]


expr3 :: Parser (RawExpr 3)
expr3 = do
  e <- RECoe <$> expr4
  choice [ try $ (pure e) >>= infixL (withAnn $ flip REAddOp <$> addOp) expr4
         , pure e
         ]


expr4 :: Parser (RawExpr 4)
expr4 = do
  e <- RECoe <$> expr5
  choice [ try $ (pure e) >>= infixL (withAnn $ flip REMulOp <$> mulOp) expr5
         , pure e
         ]


expr5 :: Parser (RawExpr 5)
expr5 = choice
  [ operator "!" *> withAnnP RENot <*> expr6
  , operator "-" *> withAnnP RENeg <*> expr6
  , RECoe <$> expr6
  ]


expr6 :: Parser (RawExpr 6)
expr6 = do
  e <- RECoe <$> expr7
  choice [ try $ (pure e) >>= infixL (withAnn $ operator "." $> REProj) ident
         , pure e
         ]


expr7 :: Parser (RawExpr 7)
expr7 = choice
  [ withAnnP REPar <*> paren expr0
  , withAnnP RELit <*> lit
  , try $ withAnnP REApp <*> ident <*> paren (sepBy expr0 (symbol ","))
  , withAnnP REVar <*> ident
  ]


rawDecl :: Parser (Id, Maybe (RawExpr 0))
rawDecl = liftA2 (,) (try $ ident <* operator "=") (Just <$> rawExpr)
  <|> liftA2 (,) ident (pure Nothing)


decl :: Parser (Id, Maybe (Expr 'Untyped))
decl = fmap (fmap entailExpr) <$> rawDecl


semicolon :: Parser ()
semicolon = void $ symbol ";"


stmt :: Parser RawStmt
stmt = choice
  [ block
  , withAnnP RSAssg <*> try (ident <* operator "=") <*> rawExpr <* semicolon
  , withAnnP RSDecl <*> type_ <*> sepBy1 rawDecl (symbol ",") <* semicolon
  , withAnnP RSIncr <*> try (ident <* operator "++") <* semicolon
  , withAnnP RSDecr <*> try (ident <* operator "--") <* semicolon
  , withAnnP RSRet <*> (try $ word "return" *> rawExpr) <* semicolon
  , withAnn (RSVRet <$ word "return") <* semicolon
  , withAnnP (\a c t me -> case me of {Nothing -> RSCond a c t; Just e -> RSCondElse a c t e})
    <*> (word "if" *> paren rawExpr) <*> stmt <*> optional (word "else" *> stmt)
  , withAnnP RSWhile <*> (word "while" *> paren rawExpr) <*> stmt
  , withAnnP RSExp <*> rawExpr <* semicolon
  , withAnn (pure RSEmpty) <* semicolon
  ]


block :: Parser RawStmt
block = withAnnP RSBlock <*> brac (many stmt)


body :: Parser (Stmt 'Untyped)
body = entailStmt <$> block


type_ :: Parser (Type 'Untyped)
type_ = choice
  [ TInt <$ word "int"
  , TBool <$ word "bool"
  , TString <$ word "string"
  , TVoid <$ word "void"
  ]


arg :: Parser (Arg 'Untyped)
arg = withAnnP Arg <*> type_ <*> ident


topDef :: Parser (TopDef 'Untyped)
topDef = choice
  [ withAnnP TopClass <*> try (word "class" *> ident) <*> (optional $ word "extends" *> ident)
    <*> brac (many classMember)
  , withAnnP TopFun <*> type_ <*> ident <*> paren (sepBy arg (symbol ",")) <*> body
  ]


classMember :: Parser (ClassMember 'Untyped)
classMember = choice
  [-- withAnnP AbstractMethod <*> (word "abstract" *> classMemberAccess) <*> classMemberPlace
   -- <*> type_ <*> ident <*> paren (sepBy arg (symbol ","))
  -- , withAnnP Method <*> classMemberAccess <*> classMemberPlace
  --   <*> type_ <*> ident <*> paren (sepBy arg (symbol ",")) <*> stmt
  -- , withAnnP Constructor <*> (word "constructor" *> classMemberAccess)
  --   <*> option ident <*> paren (sepBy arg (symbol ",")) stmt
   withAnnP Field <*> classMemberAccess <*> classMemberPlace <*> type_
    <*> sepBy1 decl (symbol ",") <* semicolon
  ]


classMemberAccess :: Parser ClassMemberAccess
classMemberAccess =
  word "public" $> Public <|> pure Private


classMemberPlace :: Parser ClassMemberPlace
classMemberPlace =
  word "static" $> Static <|> pure Dynamic

program :: Parser (Program 'Untyped)
program = Program <$> many topDef
