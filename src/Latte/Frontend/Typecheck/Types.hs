{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Latte.Frontend.Typecheck.Types where

import Latte.Frontend.AST
import Latte.Frontend.Error
import Latte.Pretty

import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map(Map)
import qualified Data.List.NonEmpty as NE
import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens

import Prelude hiding (LT, GT, EQ, (<>))


newtype ErrorPack = ErrorPack (NE.NonEmpty (Maybe Ann, Error))
  deriving (Semigroup)
instance Pretty ErrorPack where
  pPrint (ErrorPack ers) = vcat (punctuate "\n" $ fmap prnt $ NE.toList ers) where
    prnt (ma, e) = maybe empty pPrint ma <> ":" <+> pPrint e


type Typechecker =
  ExceptT ErrorPack (Reader TypecheckerEnv)
type TopTypechecker =
  Except ErrorPack


type VarEnv   = Map VarId Type
type FunEnv   = Map FunId (Type, [Type])
type ConEnv   = Map (Maybe ConstructorId) (ClassMemberAccess, [Type])
type ClassEnv = Map ClassId ClassEntry


data ClassEntry = ClassEntry
  { _ceSuper        :: Maybe ClassId
  , _ceFields       :: Map FieldId (ClassMemberAccess, Type)
  , _ceMethods      :: Map MethodId (ClassMemberAccess, (Type, [Type]))
  , _ceConstructors :: ConEnv
  }


data TypecheckerEnv = TypecheckerEnv
  { _teDefinedVars      :: VarEnv
  , _teDefinedFuns      :: FunEnv
  , _teCurrentScopeName :: Maybe String
  , _teCurrentClass     :: Maybe ClassId
  , _teClassEnv         :: ClassEnv
  , _teLoc              :: Maybe Ann
  , _teRetType          :: Maybe Type
  , _teCurrentScopeVars :: S.Set VarId
  }


initialEnv :: TypecheckerEnv
initialEnv = TypecheckerEnv
  { _teDefinedVars      = M.singleton "void" TVoid
  , _teDefinedFuns      = M.fromList $
    [ ("printInt", (TVoid, [TInt]))
    , ("printString", (TVoid, [TString]))
    , ("readInt", (TInt, []))
    , ("readString", (TString, []))
    , ("error", (TVoid, []))
    ]
  , _teCurrentScopeName = Nothing
  , _teCurrentClass     = Nothing
  , _teClassEnv         = M.empty
  , _teLoc              = Nothing
  , _teRetType          = Nothing
  , _teCurrentScopeVars = S.empty
  }


makeLensesWith abbreviatedFields ''TypecheckerEnv
makeLensesWith abbreviatedFields ''ClassEntry


