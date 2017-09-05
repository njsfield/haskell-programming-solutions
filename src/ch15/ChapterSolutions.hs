module Ch15.ChapterSolutions where

import Data.Semigroup
import Test.QuickCheck

-- Associatitity helper
semigroupAssoc
  :: (Eq m, Semigroup m)
  => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- 1
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

trivialTests :: IO ()
trivialTests = do
  putStrLn "Trivial Associaitivity Tests"
  quickCheck (semigroupAssoc :: TrivialAssoc)

-- 2
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a =>
         Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

genIdentity
  :: Arbitrary a
  => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a =>
         Arbitrary (Identity a) where
  arbitrary = genIdentity

type IdentityAssocString = Identity String -> Identity String -> Identity String -> Bool

identityTests :: IO ()
identityTests = do
  putStrLn "Identity Associativity Tests"
  quickCheck (semigroupAssoc :: IdentityAssocString)

-- 3
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
         Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

genTwo
  :: (Arbitrary a, Arbitrary b)
  => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Two a b) where
  arbitrary = genTwo

type TwoAssocString = Two String String -> Two String String -> Two String String -> Bool

twoTests :: IO ()
twoTests = do
  putStrLn "Two Associativity Tests"
  quickCheck (semigroupAssoc :: TwoAssocString)

-- 4
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

genThree
  :: (Arbitrary a, Arbitrary b, Arbitrary c)
  => Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = genThree

type ThreeAssocString = (Three String String String) -> (Three String String String) -> (Three String String String) -> Bool

threeTests :: IO ()
threeTests = do
  putStrLn "Three Associativity Tests"
  quickCheck (semigroupAssoc :: ThreeAssocString)

-- 5
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

genFour
  :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Gen (Four a b c d)
genFour = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = genFour

type FourAssocString = (Four String String String String) -> (Four String String String String) -> (Four String String String String) -> Bool

fourTests :: IO ()
fourTests = do
  putStrLn "Four Associativity Tests"
  quickCheck (semigroupAssoc :: FourAssocString)

-- 6
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  (BoolConj _) <> (BoolConj _) = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = frequency [(1, return (BoolConj False)), (1, return (BoolConj True))]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

boolConjTests :: IO ()
boolConjTests = do
  putStrLn "BoolConj Associativity Tests"
  quickCheck (semigroupAssoc :: BoolConjAssoc)

{-
 - Main
 -}
main :: IO ()
main = do
  trivialTests
  identityTests
  twoTests
  threeTests
  fourTests
  boolConjTests
