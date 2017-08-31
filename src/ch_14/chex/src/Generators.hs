module Generators where

import Test.QuickCheck

-- 1)
data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

instance Arbitrary Fool where
  arbitrary = oneof [return $ Fulse, return $ Frue]

run_fool :: IO ()
run_fool = do
  quickCheck $
    forAll
      (arbitrary :: Gen Fool)
      (\x -> x == Frue || x == Fulse)

-- 2)
data Fool'
  = Fulse'
  | Frue'
  deriving (Eq, Show)

instance Arbitrary Fool' where
  arbitrary =
    frequency [(2, return $ Frue'), (1, return $ Fulse')]

run_fool' :: IO ()
run_fool' = do
  quickCheck $
    forAll
      (arbitrary :: Gen Fool')
      (\x -> x == Frue' || x == Fulse')

main :: IO ()
main = do
  run_fool
  run_fool'
