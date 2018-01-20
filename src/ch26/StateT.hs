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
