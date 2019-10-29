{-# LANGUAGE OverloadedStrings #-}
module Latte.Parse where

import           Data.Functor(void)
import           Control.Applicative        (liftA2, (<**>))
import           Control.Applicative.Combinators.NonEmpty(sepBy1)
import           Control.Monad
import           Control.Monad.Identity
import           Data.Void
import           Data.Text(Text)
import qualified Data.Text as T
import           Data.List as DL
import           Data.Bifunctor
import           Text.Megaparsec hiding (sepBy1)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Prelude hiding (lex, LT, GT, EQ, exp)

import Latte.Types.Syntax
-- import Latte.Types.AST hiding (Op(..), Expr(..), FunDef, Program)
import Latte.PP


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


signed :: Parser Int
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
  , LBool <$> ((True <$ string "true") <|> (False <$ string "false"))
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


exp :: Parser (Expr 0)
exp = exp0


exp0 :: Parser (Expr 0)
exp0 = choice
  [ withAnnP EOr <*> (try $ exp1 <* operator "||") <*> exp0
  , ECoe <$> exp1
  ]


exp1 :: Parser (Expr 1)
exp1 = choice
  [ withAnnP EAnd <*> (try $ exp2 <* operator "&&") <*> exp1
  , ECoe <$> exp2
  ]


exp2 :: Parser (Expr 2)
exp2 = choice
  [ try $ (ECoe <$> exp3) >>= infixL (flip ERelOp <$> relOp) exp3
  , ECoe <$> exp3
  ]


exp3 :: Parser (Expr 3)
exp3 = choice
  [ try $ (ECoe <$> exp4) >>= infixL (flip EAddOp <$> addOp) exp4
  , ECoe <$> exp4
  ]


exp4 :: Parser (Expr 4)
exp4 = choice
  [ try $ (ECoe <$> exp5) >>= infixL (flip EMulOp <$> mulOp) exp5
  , ECoe <$> exp5
  ]


exp5 :: Parser (Expr 5)
exp5 = choice
  [ operator "!" *> withAnn (flip ENot <$> exp6)
  , operator "-" *> withAnn (flip ENeg <$> exp6)
  , ECoe <$> exp6
  ]


exp6 :: Parser (Expr 6)
exp6 = choice
  [ withAnnP EPar <*> paren exp0
  , withAnnP ELit <*> lit
  , try $ withAnnP EApp <*> ident <*> (paren $ sepBy exp0 (symbol ","))
  , withAnnP EVar <*> ident
  ]


decl :: Parser (Id, Maybe (Expr 0))
decl = liftA2 (,) (try $ ident <* operator "=") (Just <$> exp)
  <|> liftA2 (,) ident (pure Nothing)


semicolon :: Parser ()
semicolon = void $ symbol ";"


stmt :: Parser Stmt
stmt = choice
  [ block
  , withAnnP SAssg <*> try (ident <* operator "=") <*> exp <* semicolon
  , withAnnP SDecl <*> type_ <*> sepBy1 decl (symbol ",") <* semicolon
  , withAnnP SIncr <*> try (ident <* operator "++") <* semicolon
  , withAnnP SDecr <*> try (ident <* operator "--") <* semicolon
  , withAnnP SRet <*> (try $ word "return" *> exp) <* semicolon
  , withAnn (SVRet <$ word "return") <* semicolon
  , withAnnP (\a c t me -> case me of {Nothing -> SCond a c t; Just e -> SCondElse a c t e})
    <*> (word "if" *> paren exp) <*> stmt <*> optional (word "else" *> stmt)
  , withAnnP SWhile <*> (word "while" *> paren exp) <*> stmt
  , withAnnP SExp <*> exp <* semicolon
  , withAnn (pure SEmpty) <* semicolon
  ]


block :: Parser Stmt
block = withAnnP SBlock <*> brac (many stmt)


type_ :: Parser Type
type_ = choice
  [ withAnn (TInt <$ word "int")
  , withAnn (TBool <$ word "bool")
  , withAnn (TString <$ word "string")
  , withAnn (TVoid <$ word "void")
  ]


arg :: Parser Arg
arg = withAnnP Arg <*> type_ <*> ident


topDef :: Parser TopDef
topDef =
  (withAnnP FunDef) <*> type_ <*> ident <*> paren (sepBy arg (symbol ",")) <*> block


program :: Parser Program
program = Program <$> many topDef
