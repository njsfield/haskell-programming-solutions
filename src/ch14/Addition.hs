module Ch14.Addition where

import Test.Hspec
import Test.QuickCheck

-- Used in earlier module
dividedBy
  :: Integral a
  => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

-- Recursive multiplication
myMult
  :: (Eq a, Num a)
  => a -> a -> a
myMult num by = go num by 0
  where
    go n b total
      | b == 0 = total
      | otherwise = go n (b - 1) (total + n)

-- Tests
main :: IO ()
main =
  hspec $
  -- Addition
   do
    describe "Addition" $ do
      it "1 + 1 is greater than 1" $ do
        ((1 :: Integer) + (1 :: Integer)) >
          1 `shouldBe` True
      it "2 +  2 is equal to 4" $ do
        ((2 :: Integer) + (2 :: Integer)) `shouldBe` 4
  -- Division
    describe "Division" $ do
      it "15 divided by 3 is 5" $ do
        dividedBy (15 :: Integer) (3 :: Integer) `shouldBe`
          (5, 0)
      it "22 divided by 5 is 4 remainder 2" $ do
        dividedBy (22 :: Integer) (5 :: Integer) `shouldBe`
          (4, 2)
  -- Multiplication
    describe "Multiplication" $ do
      it "2 times 2 is 4" $ do
        myMult (2 :: Integer) (2 :: Integer) `shouldBe` 4
      it "4 times 12 is 48" $ do
        myMult (4 :: Integer) (12 :: Integer) `shouldBe` 48
  -- Addition (property tests)
    describe "Addition" $ do
      it "x + 1 is always greater than x" $ do
        property $ \x -> x + 1 > (x :: Int)
