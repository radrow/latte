{-# LANGUAGE OverloadedStrings #-}
module Latte.Parse where

import           Data.Functor(void)
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
import Latte.Types.Latte


type Parser = ParsecT Void Text Identity
runParser :: Parser a -> FilePath -> Text -> Either String a
runParser p filename inp = first
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
  lex $ try $ string o *> notFollowedBy (oneOf ("=+-/*%\\&|^<>" :: String))


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


infixL :: Parser (Ann -> a -> b -> a) -> Parser b -> a -> Parser a
infixL op p x = do
  f <- withAnn op
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


expr :: Parser (Expr 0)
expr = expr0


expr0 :: Parser (Expr 0)
expr0 = choice
  [ withAnnP EOr <*> (try $ expr1 <* operator "||") <*> expr0
  , ECoe <$> expr1
  ]


expr1 :: Parser (Expr 1)
expr1 = choice
  [ withAnnP EAnd <*> (try $ expr2 <* operator "&&") <*> expr1
  , ECoe <$> expr2
  ]


expr2 :: Parser (Expr 2)
expr2 = choice
  [ try $ (ECoe <$> expr3) >>= infixL (flip ERelOp <$> relOp) expr3
  , ECoe <$> expr3
  ]


expr3 :: Parser (Expr 3)
expr3 = choice
  [ try $ (ECoe <$> expr4) >>= infixL (flip EAddOp <$> addOp) expr4
  , ECoe <$> expr4
  ]


expr4 :: Parser (Expr 4)
expr4 = choice
  [ try $ (ECoe <$> expr5) >>= infixL (flip EMulOp <$> mulOp) expr5
  , ECoe <$> expr5
  ]


expr5 :: Parser (Expr 5)
expr5 = choice
  [ operator "!" *> withAnn (flip ENot <$> expr6)
  , operator "-" *> withAnn (flip ENeg <$> expr6)
  , ECoe <$> expr6
  ]


expr6 :: Parser (Expr 6)
expr6 = choice
  [ withAnnP EPar <*> paren expr0
  , withAnnP ELit <*> lit
  , try $ withAnnP EApp <*> ident <*> paren (sepBy expr0 (symbol ","))
  , withAnnP EVar <*> ident
  ]


decl :: Parser (Id, Maybe (Expr 0))
decl = liftA2 (,) (try $ ident <* operator "=") (Just <$> expr)
  <|> liftA2 (,) ident (pure Nothing)


semicolon :: Parser ()
semicolon = void $ symbol ";"


stmt :: Parser (Stmt (Expr 0))
stmt = choice
  [ block
  , withAnnP SAssg <*> try (ident <* operator "=") <*> expr <* semicolon
  , withAnnP SDecl <*> type_ <*> sepBy1 decl (symbol ",") <* semicolon
  , withAnnP SIncr <*> try (ident <* operator "++") <* semicolon
  , withAnnP SDecr <*> try (ident <* operator "--") <* semicolon
  , withAnnP SRet <*> (try $ word "return" *> expr) <* semicolon
  , withAnn (SVRet <$ word "return") <* semicolon
  , withAnnP (\a c t me -> case me of {Nothing -> SCond a c t; Just e -> SCondElse a c t e})
    <*> (word "if" *> paren expr) <*> stmt <*> optional (word "else" *> stmt)
  , withAnnP SWhile <*> (word "while" *> paren expr) <*> stmt
  , withAnnP SExp <*> expr <* semicolon
  , withAnn (pure SEmpty) <* semicolon
  ]


block :: Parser (Stmt (Expr 0))
block = withAnnP SBlock <*> brac (many stmt)


type_ :: Parser Type
type_ = choice
  [ TInt <$ word "int"
  , TBool <$ word "bool"
  , TString <$ word "string"
  , TVoid <$ word "void"
  ]


arg :: Parser Arg
arg = withAnnP Arg <*> type_ <*> ident


topDef :: Parser TopDef
topDef =
  (withAnnP FunDef) <*> type_ <*> ident <*> paren (sepBy arg (symbol ",")) <*> block


program :: Parser Program
program = Program <$> many topDef
