{-# LANGUAGE ViewPatterns #-}

module Ch16.FunctorLaws where

import           Test.QuickCheck
import           Test.QuickCheck.Function

-- Identity
functorIdentity :: (Functor f, Eq (f a))
                => f a
                -> Bool
functorIdentity f =
  fmap id f == f

-- Compose
functorCompose :: (Functor f, Eq (f c))
                => f a
                -> Fun a b
                -> Fun b c
                -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)
