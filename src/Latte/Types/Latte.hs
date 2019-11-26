module Latte.Types.Latte
  ( Lit(..)
  , Id(..)
  , Ann(..)
  , fakeAnn
  , Type(..)
  , Arg(..)
  , HasAnn(..)
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

fakeAnn :: Ann
fakeAnn = Ann "no_file" 0 0

data Type = TInt | TString | TBool | TVoid
  deriving (Show, Eq)

data Arg = Arg Ann Type Id
  deriving (Show)


class HasAnn a where
  getAnn :: a -> Ann
