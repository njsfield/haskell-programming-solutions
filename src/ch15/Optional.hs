module Ch15.Optional where

import Data.Monoid
import Test.QuickCheck

{-
 - Optional Monoid
 -}

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty                    = Nada
  mappend Nada Nada         = Nada
  mappend (Only a) Nada     = Only a
  mappend Nada (Only a)     = Only a
  mappend (Only a) (Only b) = Only (a <> b)

genOnly :: Arbitrary a => Gen (Optional a)
genOnly = do
  a <- arbitrary
  return (Only a)

genOptional :: (Arbitrary a) => Gen (Optional a)
genOptional =
  frequency [ (1, return Nada)
            , (1, genOnly)
            ]

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = genOptional

main :: IO ()
main
  -- 1
 = do
  putStrLn "1. Only (Sum 1) `mappend` Only (Sum 1)) == Only (Sum {getSum = 2})"
  putStrLn $ show (Only (Sum 1) `mappend` Only (Sum 1))
  -- 2
  putStrLn "2. Only (Product 4) `mappend` Only (Product 2)) == Only (Product {getProduct = 8})"
  putStrLn $ show (Only (Product 4) `mappend` Only (Product 2))
  -- 3
  putStrLn "3. Only (Sum 1) `mappend` Noda == Only (Sum {getSum = 1})"
  putStrLn $ show (Only (Sum 1) `mappend` Nada)
  -- 4
  putStrLn "4. Only [1] `mappend` noda == Only [1]"
  putStrLn $ show (Only [1] `mappend` Nada)
  -- 5
  putStrLn "5. Nada `mappend` Only (Sum 1) == Only (Sum {getSum = 1})"
  putStrLn $ show (Nada `mappend` (Only (Sum 1)))
