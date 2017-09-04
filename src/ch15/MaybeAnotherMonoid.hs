module Ch15.MaybeAnotherMonoid where

import Ch15.Optional
import Ch15.QuickChecking
       (monoidAssoc, monoidLeftIdentity, monoidRightIdentity)

import Data.Monoid
import Test.QuickCheck

newtype First' a =
        First' { getFirst' :: Optional a} 
        deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) 
          (First' Nada)     = First' {getFirst' = Nada}
  mappend (First' (Only a)) 
          (First' Nada)     = First' {getFirst' = Only a}
  mappend (First' (Only a)) 
          (First' (Only b)) = First' {getFirst' = Only a}
  mappend (First' Nada) 
          (First' (Only a)) = First' {getFirst' = Only a}

-- newtype requires its child to have Arbitrary
-- instance (see Optional.hs)
genFirst :: Arbitrary a => Gen (First' a)
genFirst = do
  a <- arbitrary
  return First' { getFirst' = a }

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst
  
firstMappend :: First' a 
             -> First' a 
             -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

main :: IO () 
main = do
  quickCheck (monoidAssoc :: FirstMappend) 
  quickCheck (monoidLeftIdentity :: FstId) 
  quickCheck (monoidRightIdentity :: FstId)
