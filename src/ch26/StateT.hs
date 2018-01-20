{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Ch26.StateT where

import Control.Arrow


newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sms) =
    StateT $ (fmap . fmap) (first f) sms

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ pure . (a,)

  (<*>) :: StateT s m (a -> b)
        -> StateT s m a
        -> StateT s m b
  StateT mf <*> StateT ma =
    StateT $ \s -> do
      (fa, s')  <- mf s
      (a', s'') <- ma s'
      return (fa a', s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  StateT sma >>= f =
    StateT $ \s -> do
      (a, s') <- sma s
      runStateT (f a) s'




-- newtype EitherT e m a =
--   EitherT { runEitherT :: m (Either e a) }
--
-- instance Functor m => Functor (EitherT e m) where
--   fmap f (EitherT mea) =
--     EitherT $ (fmap . fmap) f mea
--
-- instance Applicative m => Applicative (EitherT e m) where
--   pure a = EitherT $ pure (Right a)
--   (EitherT mf) <*> (EitherT ma) =
--     EitherT $ fmap (<*>) mf <*> ma
--
-- instance Monad m => Monad (EitherT e m) where
--   return = pure
--   (EitherT mv) >>= f =
--     EitherT $ do
--       v <- mv
--       case v of
--         Left e -> return (Left e)
--         Right a -> runEitherT (f a)
--
-- swapEither :: Either e a -> Either a e
-- swapEither (Left e) = Right e
-- swapEither (Right a) = Left a
--
-- swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
-- swapEitherT (EitherT mea) =
--   EitherT $ fmap swapEither mea
--
-- eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
-- eitherT leftF rightF (EitherT mea) = do
--   v <- mea
--   case v of
--     Left e -> leftF e
--     Right a -> rightF a
