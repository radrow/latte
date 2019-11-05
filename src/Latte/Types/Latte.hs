module Latte.Types.Latte
  ( Lit(..)
  , Id(..)
  , Ann(..)
  , Type(..)
  ) where

data Lit
  = LInt Integer
  | LString String
  | LBool Bool
  deriving (Show)

newtype Id = Id {iName :: String}
  deriving (Show, Eq, Ord)

data Ann = Ann { file :: FilePath, line :: Int, column :: Int }
  deriving (Show)

data Type = TInt | TString | TBool | TFun [Type] Type | TVoid
  deriving (Show, Eq)
