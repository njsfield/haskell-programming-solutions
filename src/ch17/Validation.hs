module Ch17.Validation where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Validation' e a =
    Failure' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation' e) where
  fmap f (Failure' e) = Failure' e
  fmap f (Success' a) = Success' (f a)

instance Monoid e => Applicative (Validation' e) where
  pure a = Success' a
  (<*>) (Failure' e) (Failure' e') = Failure' (e <> e')
  (<*>) (Failure' e) _             = Failure' e
  (<*>) _ (Failure' e)             = Failure' e
  (<*>) (Success' f) (Success' a)  = Success' (f a)

instance (Eq a, Eq b) => EqProp (Validation' a b)
  where (=-=) = eq

genValidation :: (Arbitrary a, Arbitrary b) => Gen (Validation' a b)
genValidation = do
  a <- arbitrary
  b <- arbitrary
  elements [ Failure' a, Success' b]

instance (Arbitrary a, Arbitrary b) => (Arbitrary (Validation' a b)) where
  arbitrary = genValidation

validationSpec :: IO ()
validationSpec = do
  quickBatch $ applicative (undefined :: Validation' String (Int, String, Char))
