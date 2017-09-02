module Ch08.NumbersToWords where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n
  | n == 0    = "zero"
  | n == 1    = "one"
  | n == 2    = "two"
  | n == 3    = "three"
  | n == 4    = "four"
  | n == 5    = "five"
  | n == 6    = "six"
  | n == 7    = "seven"
  | n == 8    = "eight"
  | n == 9    = "nine"
  | otherwise = [] 

digits :: Int -> [Int]
digits n 
  | n == 0    = []
  | otherwise = digits (div n 10) ++ [mod n 10]

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits 

