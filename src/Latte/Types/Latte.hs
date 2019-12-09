{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module Latte.Types.Latte
  ( Lit(..)
  , Id(..)
  , Ann(..)
  , fakeAnn
  , Type(..)
  , Arg(..)
  , HasAnn(..)
  , Stage(..)
  , coerceType
  , Unresolved
  ) where

data Stage = Untyped | Typed | Resolved
class Unresolved (s :: Stage) where
instance Unresolved 'Untyped where
instance Unresolved 'Typed where

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

data Type (s :: Stage) where
  TVoid :: Type s
  TInt :: Type s
  TBool :: Type s
  TString :: Type s
  TObj :: Unresolved s => Id -> Type s
  TTuple :: [Type 'Resolved] -> Type 'Resolved
deriving instance Show (Type s)
deriving instance Eq (Type s)
coerceType :: Type 'Untyped -> Type 'Typed
coerceType t = case t of
  TVoid -> TVoid
  TInt -> TInt
  TBool -> TBool
  TString -> TString
  TObj o -> TObj o

data Arg (s :: Stage) = Arg Ann (Type s) Id
  deriving (Show)


class HasAnn a where
  getAnn :: a -> Ann
