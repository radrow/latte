module Latte.Typecheck where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map as M
import           Data.Map(Map)

import Latte.Types.Syntax(Type, Id)


type Typechecker = ExceptT String (Reader (Map Id Type))
