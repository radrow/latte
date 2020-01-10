module Control.Monad.MultiError where

import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.Trans
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor

-- newtype MultiErrorT e m a =
--   MultiErrorT { runMultiErrorT :: Maybe (m a) -> m (Either (NE.NonEmpty e) a) }

-- instance Monad m => Functor (MultiErrorT e m) where
--   fmap f (MultiErrorT me) = MultiErrorT $ \fl ->
--     fmap (fmap f) (me fl)

-- instance Monad m => Applicative (MultiErrorT e m) where
--   pure a = MultiErrorT $ (Nothing, pure (Right a))
--   (MultiErrorT (mflf, mf)) <*> (MultiErrorT (mfla, ma)) = MultiErrorT $ do
--     ef <- mf
--     case ef of
--       Left erf -> case mflf of
--         Nothing -> pure ()

-- instance Monad m => Monad (MultiErrorT e m) where
--   return = pure
--   (MultiErrorT mf) >>= f = MultiErrorT $ do
--     ea <- mf
--     case ea of
--       Left es -> pure $ Left es
--       Right a -> runMultiErrorT $ f a

-- instance MonadTrans (MultiErrorT e) where
--   lift f = MultiErrorT $ Right <$> f

-- throwError :: Applicative m => e -> MultiErrorT e m a
-- throwError e = MultiErrorT (pure (Left (pure e)))

-- catchErrors :: Monad m =>
--   MultiErrorT e m a -> (NE.NonEmpty e -> MultiErrorT e m a) -> MultiErrorT e m a
-- catchErrors f c = MultiErrorT $ do
--   mf <- runMultiErrorT f
--   case mf of
--     Left es -> runMultiErrorT $ c es
--     Right a -> pure (Right a)

