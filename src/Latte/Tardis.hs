{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Latte.Tardis(Tardis) where

import Control.Monad.Tardis
import Control.Monad.Except
import Control.Monad.Reader

instance MonadTardis bw fw m => MonadTardis bw fw (ExceptT e m) where
  getFuture = lift getFuture
  getPast = lift getPast
  sendFuture f = lift (sendFuture f)
  sendPast p = lift (sendPast p)

instance MonadTardis bw fw m => MonadTardis bw fw (ReaderT e m) where
  getFuture = lift getFuture
  getPast = lift getPast
  sendFuture f = lift (sendFuture f)
  sendPast p = lift (sendPast p)
