module Ch15.ChapterSolutions where

import Data.Semigroup
import Test.QuickCheck

-- Associatitity helper

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = 
  (a <> (b <> c)) == ((a <> b) <> c)

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

instance Semigroup a => Semigroup (Identity a) where 
  (Identity a) <> (Identity a') = Identity (a <> a')

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

type IdentityAssocString = 
     Identity String 
  -> Identity String 
  -> Identity String 
  -> Bool

identityTests :: IO ()
identityTests = do 
  putStrLn "Identity Associativity Tests"
  quickCheck (semigroupAssoc :: IdentityAssocString)

-- 3

data Two a b = 
  Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do 
  a <- arbitrary
  b <- arbitrary
  return (Two a b) 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

type TwoAssocString = 
     (Two String String)
  -> (Two String String)
  -> (Two String String)
  -> Bool

twoTests :: IO ()
twoTests = do 
  putStrLn "Two Associativity Tests"
  quickCheck (semigroupAssoc :: TwoAssocString)

main :: IO ()
main = do
  trivialTests
  identityTests
  twoTests
