{-# LANGUAGE UndecidableInstances #-}
module Latte.Semantics.IR where

import qualified Latte.Frontend.IR as IR

import Control.Monad.RWS
import Control.Lens
import qualified Data.HashTable.IO as H
import qualified Data.Map as M

data Val = VInt Int | VStr String

type Mem = H.CuckooHashTable Int Val
type Namespace = M.Map Int Int

data St = St
  { _stSupply :: Int
  }

data Env = Env
  { _envMem :: Mem
  , _envNamespace :: Namespace
  }

makeLensesWith abbreviatedFields ''Env
makeLensesWith abbreviatedFields ''St

type Eval = RWST Env () St IO

getLoc :: IR.VarId -> Eval Int
getLoc (IR.VarId c) = views namespace (M.! c)

getVal :: Int -> Eval Val
getVal i = do
  m <- view mem
  mv <- liftIO $ H.lookup m i
  case mv of
    Nothing -> error $ "unknown id: " ++ show i
    Just x -> return x

getValById :: IR.VarId -> Eval Val
getValById = getLoc >=> getVal

withId :: IR.VarId -> Int -> Eval a -> Eval a
withId (IR.VarId i) l =
  local (over namespace (M.insert i l))

writeVal :: Int -> Val -> Eval ()
writeVal l v = do
  m <- view mem
  liftIO $ H.insert m l v

