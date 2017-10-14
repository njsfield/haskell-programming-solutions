module Ch17.ChapterExercises where

import           Control.Applicative      (liftA3)
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- Instances
-- 1
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

instance Eq a => EqProp (Pair a) where (=-=) = eq

-- 2
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a a') = Two a (f a')

instance Monoid a => Applicative (Two a) where
  pure a = Two mempty a
  (<*>) (Two a f) (Two a' b) = Two (a <> a') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

-- 3
data Three a b = Three a b b deriving (Eq, Show)

instance Functor (Three a) where
  fmap f (Three a b b') = Three a (f b) (f b')

instance Monoid a => Applicative (Three a) where
  pure a = Three mempty a a
  (<*>) (Three a f f') (Three a' b b') = Three (a <> a') (f b) (f' b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Three a b b)

instance (Eq a, Eq b) => EqProp (Three a b) where (=-=) = eq

-- 4
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four a b c f) (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

-- 5
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' a b c f) (Four' a' b' c' d) = Four' (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four' a b c d)

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

testSpec :: IO ()
testSpec = do
  quickBatch $ applicative (Pair ("a", "2", "3") ("1","2","3"))
  quickBatch $ applicative (Two ("b", "b", "b")("b", "b", "b"))
  quickBatch $ applicative (Three "b" ("f", "f", "a") ("c", "h", "b"))
  quickBatch $ applicative (Four "b" "a" "c" ("c", "h", "a"))
  quickBatch $ applicative (Four' "a" "1" "1" ("c", "h", "a"))


-- Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

combinations :: [(Char, Char, Char)]
combinations = combos stops vowels stops


