module WordNumber where

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Test.Hspec

digitToWord :: Int -> Maybe String
digitToWord 0 = Just "zero"
digitToWord 1 = Just "one"
digitToWord 2 = Just "two"
digitToWord 3 = Just "three"
digitToWord 4 = Just "four"
digitToWord 5 = Just "five"
digitToWord 6 = Just "six"
digitToWord 7 = Just "seven"
digitToWord 8 = Just "eight"
digitToWord 9 = Just "nine"
digitToWord _ = Nothing

digits :: Int -> [Int]
digits x
  | x == 0    = []
  | otherwise = digits (div x 10) ++ [mod x 10]

wordNumber :: Int -> String
wordNumber =
  intercalate "-" 
  . map (fromJust . digitToWord) . digits

main :: IO ()
main =
  hspec $ do
    
    describe "digitToWord" $ do
      it "returns zero for 0" $ do
        digitToWord 0 `shouldBe` (Just "zero")
      it "returns one for 1" $ do
        digitToWord 1 `shouldBe` (Just "one")

    describe "digits" $ do
      it "returns [1] for 1  " $ do 
        digits 1 `shouldBe` [1]
      it "returns [1, 0, 0] for 100" $ do
        digits 100 `shouldBe` [1, 0, 0]

    describe "wordNumber" $ do
      it "one-zero-zero given 100" $ do
        wordNumber 100 `shouldBe` "one-zero-zero"
      it "nine-zero-zero-one for 9001" $ do
        wordNumber 9001 `shouldBe` "nine-zero-zero-one"
