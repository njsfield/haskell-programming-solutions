module Ch14.UsingQuickCheck where

import Test.QuickCheck
import Data.List (sort)

-- 1)
half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = halfIdentity x == x

run_halfIdentity :: IO ()
run_halfIdentity = do  
  putStrLn "Half identity tests"
  quickCheck (prop_halfIdentity :: Float -> Bool) 
  quickCheck (prop_halfIdentity :: Double -> Bool)

-- 2)
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = 
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

prop_listOrdered :: (Ord a) => [a] -> Bool 
prop_listOrdered = listOrdered . sort

run_listOrdered :: IO()
run_listOrdered = do
  putStrLn "Ordered List tests"
  quickCheck (prop_listOrdered :: [Int] -> Bool)
  quickCheck (prop_listOrdered :: [Char] -> Bool)


-- 3) 

prop_plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool 
prop_plusAssociative x y z = 
  x + (y + z) == (x + y) + z

prop_plusCommutative :: (Eq a, Num a) => a -> a -> Bool 
prop_plusCommutative x y =
  x + y == y + x

run_plusses :: IO()
run_plusses = do
  putStrLn "Plus Tests"
  quickCheck (prop_plusAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (prop_plusCommutative :: Int -> Int -> Bool)

-- 4)

prop_multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_multAssociative x y z =
  x * (y * z) == (x * y) * z

prop_multCommutative :: (Eq a, Num a) => a -> a -> Bool
prop_multCommutative x y =
  x * y == y * x

run_mults :: IO()
run_mults = do
  putStrLn "Mult Tests"
  quickCheck (prop_multAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (prop_multCommutative :: Int -> Int -> Bool)

-- 5) (Fails when y == 0)

prop_quotRemAssociative :: (Eq a, Integral a) => a -> a -> Bool
prop_quotRemAssociative x y =
  y == 0 || (quot x y)*y + (rem x y) == x

prop_divModAssociative :: (Eq a, Integral a) => a -> a -> Bool
prop_divModAssociative x y =
 y == 0 || (div x y)*y + (mod x y) == x

run_quotRemDivMods :: IO()
run_quotRemDivMods = do
  putStrLn "Quot Rem Tests"
  quickCheck (prop_quotRemAssociative :: Int -> Int -> Bool)
  putStrLn "Div Mod Tests"
  quickCheck (prop_divModAssociative :: Int -> Int -> Bool)

-- 6) (^ Can be neither associative or commutative for all values)

prop_powerAssociative :: (Eq a, Integral a) => a -> a -> a -> Bool
prop_powerAssociative x y z = 
  (x ^ y) ^ z == x ^ (y ^ z)

prop_powerCommutative :: (Eq a, Integral a) => a -> a -> Bool
prop_powerCommutative x y = 
  x ^ y == y ^ x

run_powers :: IO()
run_powers = do
  putStrLn "Power associativity tests"
  quickCheck (prop_powerAssociative :: Int -> Int -> Int -> Bool) -- Fails on 0 0 0 
  quickCheck (prop_powerCommutative :: Int -> Int -> Bool)        -- Fails on 0 1

-- 7)
prop_doubleReverse :: (Eq a) => [a] -> Bool
prop_doubleReverse lst = (reverse . reverse $ lst) == id lst

run_doubleReverse :: IO()
run_doubleReverse = do
  putStrLn "Reverse Property Tests"
  quickCheck (prop_doubleReverse :: [String] -> Bool)
  quickCheck (prop_doubleReverse :: [Int] -> Bool)

-- 8)
prop_dollar :: Eq a => a -> Bool 
prop_dollar a =
  (id $ a) == id a 

run_dollar :: IO()
run_dollar = do
  putStrLn "$ Property Tests"
  quickCheck (prop_dollar :: Int -> Bool) 
  quickCheck (prop_dollar :: Char -> Bool)

prop_compose :: Eq a => a -> Bool
prop_compose a =
  (id . id $ a)  == id (id a)

run_compose :: IO()
run_compose = do
  putStrLn "Compose Property Tests"
  quickCheck (prop_compose :: Int -> Bool)
  quickCheck (prop_compose :: Char -> Bool)

-- 9)

run_cons :: IO()
run_cons = do
  putStrLn "Cons Property Tests"
  quickCheck $
    forAll (arbitrary :: Gen [Char])
    (\x -> (foldr (:) [] x) == ((++) [] x))

run_concat :: IO()
run_concat = do
  putStrLn "Concat Tests"
  quickCheck $ 
    forAll (arbitrary :: Gen [[Int]]) 
    (\x -> (foldr (++) [] x) == concat x)

-- 10) (Fails for negative integers)

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

run_length :: IO()
run_length = do
  putStrLn "Length Tests"
  quickCheck $
    forAll (genTuple :: Gen (Int, [Int]))
    (\(x, xs) -> (length (take x xs)) == x)

-- 11)

run_readShow :: IO()
run_readShow = do
  putStrLn "Read/Show Tests"
  quickCheck $
    forAll (arbitrary :: Gen [Int])
    (\x -> (read (show x)) == x)

{-
 - Failure
 - sqrt :: Floating a -> a -> a
 -
 - Fails due to low fractional bits 
 - for Floats & Doubles (resulting in 
 - truncations after evaluating)
 -
 - Use FixedPoint package 
 -}
 
run_sqrt :: IO()
run_sqrt = do
  putStrLn "Square Root Tests"
  quickCheck $
    forAll (arbitrary :: Gen Double)
    (\x -> ((*) x . sqrt $ x) == x)


main :: IO()
main = do
  run_mults
  run_listOrdered
  run_halfIdentity
  run_plusses
  run_quotRemDivMods
  run_powers
  run_doubleReverse
  run_dollar
  run_compose
  run_cons
  run_concat
  run_length
  run_readShow
  run_sqrt
  
