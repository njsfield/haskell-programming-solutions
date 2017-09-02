module Ch10.Scans where

{-
 - Scans Exercises
 -}

fibs    = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

-- 1
fibs20  = take 20 fibs

-- 2
fibsLessThan100 = takeWhile (<100) fibs

-- 3
fact    = scanl (*) 1 [1..]
factN x = fact !! x

