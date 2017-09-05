module Ch15.Semigroups where

import           Data.Semigroup
import           Test.QuickCheck

-- Associatitity helper
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  a <> (b <> c) == (a <> b) <> c

-- 1
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc =
     Trivial ->
     Trivial ->
     Trivial -> Bool

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

genIdentity :: Arbitrary a
            => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

type IdentityAssocString =
     Identity String ->
     Identity String ->
     Identity String -> Bool

identityTests :: IO ()
identityTests = do
  putStrLn "Identity Associativity Tests"
  quickCheck (semigroupAssoc :: IdentityAssocString)

-- 3
data Two a b =
     Two a b
     deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
         Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Two a b) where
  arbitrary = genTwo

type TwoAssocString =
     Two String String ->
     Two String String ->
     Two String String ->
     Bool

twoTests :: IO ()
twoTests = do
  putStrLn "Two Associativity Tests"
  quickCheck (semigroupAssoc :: TwoAssocString)

-- 4
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance (Semigroup a,
          Semigroup b,
          Semigroup c) =>
         Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c)
         => Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = genThree

type ThreeAssocString =
     Three String String String ->
     Three String String String ->
     Three String String String ->
     Bool

threeTests :: IO ()
threeTests = do
  putStrLn "Three Associativity Tests"
  quickCheck (semigroupAssoc :: ThreeAssocString)

-- 5
data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance (Semigroup a,
          Semigroup b,
          Semigroup c,
          Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

genFour :: (Arbitrary a,
            Arbitrary b,
            Arbitrary c,
            Arbitrary d)
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

type FourAssocString =
     Four String String String String ->
     Four String String String String ->
     Four String String String String ->
     Bool

fourTests :: IO ()
fourTests = do
  putStrLn "Four Associativity Tests"
  quickCheck (semigroupAssoc :: FourAssocString)

-- 6
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  BoolConj _    <> BoolConj _    = BoolConj True

instance Arbitrary BoolConj where
  arbitrary =
    frequency [(1, return (BoolConj False)),
               (1, return (BoolConj True))]

type BoolConjAssoc =
     BoolConj ->
     BoolConj ->
     BoolConj ->
     Bool

boolConjTests :: IO ()
boolConjTests = do
  putStrLn "BoolConj Associativity Tests"
  quickCheck (semigroupAssoc :: BoolConjAssoc)

-- 7
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj True  <> BoolDisj False = BoolDisj True
  BoolDisj False <> BoolDisj True  = BoolDisj True
  BoolDisj _     <> BoolDisj _     = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary =
    frequency [(1, return (BoolDisj False)),
               (1, return (BoolDisj True))]

type BoolDisjAssoc =
     BoolDisj ->
     BoolDisj ->
     BoolDisj ->
     Bool

boolDisjTests :: IO ()
boolDisjTests = do
  putStrLn "BoolDisj Associativity Tests"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)

-- 8
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  Fst a <> Snd b = Snd b
  Fst a <> Fst b = Fst b
  Snd a <> Fst b = Snd a
  Snd a <> Snd b = Snd a

genOr :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
genOr = do
  a <- arbitrary
  b <- arbitrary
  frequency [(1, return (Fst a)),
             (1, return (Snd b))]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = genOr

type OrAssoc =
     Or String String ->
     Or String String ->
     Or String String ->
     Bool

orTests :: IO ()
orTests = do
  putStrLn "Or Associativity Tests"
  quickCheck (semigroupAssoc :: OrAssoc)

-- 9
newtype Combine a b =
        Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

-- 10
newtype Comp a =
        Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> Comp g = Comp (g . f)

-- 11
data Validation a b
  = Failure' a
  | Success' b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Success' a <> Success' _  = Success' a
  _          <> Failure' b  = Failure' b
  Failure' a <> _           = Failure' a

validationGen :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
validationGen = do
  a <- arbitrary
  b <- arbitrary
  elements [ Failure' a, Success' b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = validationGen

type ValidationAssoc =
  Validation String Int ->
  Validation String Int ->
  Validation String Int ->
  Bool

validationTests :: IO ()
validationTests = do
  putStrLn "Validation Associativity Tests"
  quickCheck (semigroupAssoc :: ValidationAssoc)

-- 13
newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b =>
  Semigroup (AccumulateRight a b) where
  AccumulateRight (Success' b) <> AccumulateRight (Success' b') = AccumulateRight (Success' (b <> b'))
  AccumulateRight (Success' _) <> AccumulateRight (Failure' a)  = AccumulateRight (Failure' a)
  AccumulateRight (Failure' a) <> _                             = AccumulateRight (Failure' a)

accumulateGen :: (Arbitrary a, Arbitrary b) => Gen (AccumulateRight a b)
accumulateGen = do
  a <- validationGen
  b <- validationGen
  frequency [(1, return (AccumulateRight a)),  (2, return (AccumulateRight b))]

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = accumulateGen

type AccumulateAssoc =
  AccumulateRight String (Sum Int) ->
  AccumulateRight String (Sum Int) ->
  AccumulateRight String (Sum Int) ->
  Bool

accumulateTests :: IO ()
accumulateTests = do
  putStrLn "AccumulateRight Associativity Tests"
  quickCheck (semigroupAssoc :: AccumulateAssoc)

-- 14
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Success' b) <> AccumulateBoth (Success' b') = AccumulateBoth (Success' (b <> b'))
  AccumulateBoth (Failure' a) <> AccumulateBoth (Failure' a') = AccumulateBoth (Failure' (a <> a'))
  _                           <> AccumulateBoth (Failure' a)  = AccumulateBoth (Failure' a)
  AccumulateBoth (Failure' a) <> _                            = AccumulateBoth (Failure' a)

accumulateBothGen :: (Arbitrary a, Arbitrary b) => Gen (AccumulateBoth a b)
accumulateBothGen = do
  a <- validationGen
  b <- validationGen
  frequency [(1, return (AccumulateBoth a)),  (1, return (AccumulateBoth b))]

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = accumulateBothGen

type AccumulateBothAssoc =
  AccumulateBoth String (Sum Int) ->
  AccumulateBoth String (Sum Int) ->
  AccumulateBoth String (Sum Int) ->
  Bool

accumulateBothTests :: IO ()
accumulateBothTests = do
  putStrLn "AccumulateBoth Associativity Tests"
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)

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
  boolDisjTests
  orTests
  validationTests
  accumulateTests
  accumulateBothTests
