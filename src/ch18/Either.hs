module Ch17.Either where

import           Control.Applicative
import           Data.Monoid              (Monoid, (<>))
import           Test.QuickCheck          (Arbitrary, arbitrary, elements)
import           Test.QuickCheck.Checkers (EqProp, eq, quickBatch, (=-=))
import           Test.QuickCheck.Classes  (applicative, monad)


data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure a = Second a
  (<*>) (First a) _           = First a
  (<*>) _ (First a)           = First a
  (<*>) (Second f) (Second a) = Second (f a)

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _  = First a
  (>>=) (Second a) f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

trigger :: Sum String (String, String, String)
trigger = undefined

test :: IO()
test = do
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
