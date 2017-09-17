{-# LANGUAGE ViewPatterns #-}

module Ch16.Instances where

import           Ch16.FunctorLaws
import           Test.QuickCheck
import           Test.QuickCheck.Function

-- 1
newtype Identity a =
        Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

type IdentityFC =  Identity Int
                -> Fun Int Int
                -> Fun Int Int
                -> Bool

-- 2
data Pair a =
     Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

genPair :: Arbitrary a => Gen (Pair a)
genPair = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = genPair

type PairFC =  Pair String
            -> Fun String String
            -> Fun String String
            -> Bool

-- 3
data Two a b =
     Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

type TwoFC =  Two Int String
           -> Fun String String
           -> Fun String String
           -> Bool

-- 4
data Three a b c =
     Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

genThree :: ( Arbitrary a
            , Arbitrary b
            , Arbitrary c )
         => Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a
        , Arbitrary b
        , Arbitrary c)
        => Arbitrary (Three a b c)  where
  arbitrary = genThree

type ThreeFC = Three Int String Char
           -> Fun Char Char
           -> Fun Char Char
           -> Bool

-- 5
data Three' a b =
     Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

genThree' :: ( Arbitrary a
             , Arbitrary b)
         => Gen (Three' a b)
genThree' = do
  a <- arbitrary
  b <- arbitrary
  return (Three' a b b)

instance (Arbitrary a
        , Arbitrary b)
        => Arbitrary (Three' a b)  where
  arbitrary = genThree'

type Three'FC = Three' Int Char
           -> Fun Char Char
           -> Fun Char Char
           -> Bool

-- 6
data Four a b c d =
     Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

genFour :: ( Arbitrary a
           , Arbitrary b
           , Arbitrary c
           , Arbitrary d)
         => Gen (Four a b c d)
genFour = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

instance (Arbitrary a
        , Arbitrary b
        , Arbitrary c
        , Arbitrary d)
        => Arbitrary (Four a b c d)  where
  arbitrary = genFour

type FourFC = Four Int Char String String
           -> Fun String String
           -> Fun String String
           -> Bool

-- 6
data Four' a b =
     Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

genFour' :: ( Arbitrary a
           , Arbitrary b)
         => Gen (Four' a b)
genFour' = do
  a <- arbitrary
  b <- arbitrary
  return (Four' a a a b)

instance (Arbitrary a
        , Arbitrary b)
        => Arbitrary (Four' a b)  where
  arbitrary = genFour'

type Four'FC = Four' Int String
           -> Fun String String
           -> Fun String String
           -> Bool
main :: IO ()
main = do
  -- 1
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck (functorCompose :: IdentityFC)
  -- 2
  quickCheck (functorIdentity :: Pair Int -> Bool)
  quickCheck (functorCompose :: PairFC)
  -- 3
  quickCheck (functorIdentity :: Two Int String -> Bool)
  quickCheck (functorCompose :: TwoFC)
  -- 4
  quickCheck (functorIdentity :: Three Int String Char -> Bool)
  quickCheck (functorCompose :: ThreeFC)
  -- 5
  quickCheck (functorIdentity :: Three' Int String -> Bool)
  quickCheck (functorCompose :: Three'FC)
  -- 6
  quickCheck (functorIdentity :: Four Int String Char Char -> Bool)
  quickCheck (functorCompose :: FourFC)
  -- 7
  quickCheck (functorIdentity :: Four' Int String -> Bool)
  quickCheck (functorCompose :: Four'FC)


