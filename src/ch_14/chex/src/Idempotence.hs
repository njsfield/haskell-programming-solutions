module Idempotence where

import Data.Char (toUpper)
import Data.List (sort)
import Test.QuickCheck

twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

-- 1)
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (h:t) = toUpper h : t

run_capitalizeWord :: IO ()
run_capitalizeWord = do
  quickCheck $
    forAll
      (arbitrary :: Gen String)
      (\s ->
         (capitalizeWord s == twice capitalizeWord s) &&
         (capitalizeWord s == fourTimes capitalizeWord s))

-- 2)
run_sort :: IO ()
run_sort = do
  quickCheck $
    forAll
      (arbitrary :: Gen [String])
      (\strs ->
         (sort strs == twice sort strs) &&
         (sort strs == fourTimes sort strs))

main :: IO ()
main = do
  run_capitalizeWord
  run_sort
