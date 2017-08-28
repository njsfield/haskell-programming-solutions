{-# LANGUAGE NoMonomorphismRestriction #-}

module ChapterExercises where

{-
 - Multiple Choice
 - ===============
 -
 - 1. A value of type [a] is
 -    c) a list whose elements are all of some type a
 -
 - 2. A function of type [[a]] -> [a] could
 -    a) take a list of strings as an argument   
 -
 - 3. A function of type [a] -> Int -> a
 -    b) returns one element of type a from a list
 -
 - 4. A function of type (a, b) -> a
 -    c) takes a tuple argument and returns the first value
 -}


{-
 - Determine the type
 - ==================
 -}

-- 1)
-- a
--det_a :: Num a => a
det_a = (* 9) 6

-- b
--det_b :: Num a => (a, [Char]) 
det_b = head [(0, "doge"), (1, "kitteh")]

-- c
--det_c :: (Integer, [Char])
det_c = head[(0 :: Integer, "doge"),(1, "kitteh")]

-- d
--det_d :: Bool
det_d = if False then True else False

-- e
--det_e :: Int
det_e = length [1, 2, 3, 4, 5]

-- f
--det_f :: Bool
det_f = (length [1,2,3,4]) > (length "TACOCAT")

{- 2.
 -
 - x = 5
 - y = x + 5
 - w = y * 10
 -
 - w :: Num a => a
 -}

{- 3.
 -
 - x = 5
 - y = x + 5
 - z y = y * 10
 - 
 - z :: Num a => a -> a
 -}

{- 4. 
 -
 - x = 5
 - y = x + 5
 - f = 4 / y
 -
 - f :: Fractional a => a
 -}

{- 5. 
 - 
 - x = "Julie"
 - y = " <3 "
 - z = "Haskell"
 - f = x ++ y ++ z
 - 
 - f :: [Char]
 -}


{-
 - Does it compile?
 - ================
 -
 - 1. bigNum  = (^) 5 $ 10 
 -    wahoo = bigNum $ 10
 - 
 - Error - bigNum is not a function
 -
 - 2. x = print
 -    y = print "woohoo!"
 -    z = x "hello world"
 -
 - Fine
 -
 - 3. a = (+) 
 -    b = 5
 -    c = b 10
 -    d = c 200
 -
 - Error - b is not a function
 -
 -
 - 3. a = 12 + b
 -    b = 10000 * c
 -
 - Error if c not defined
 -}

{-
 - Type variable of specific type constructor?
 - ===========================================
 - 
 - FP = Fully polymorphic
 - CP = Constrained polymorphic type variable
 - CT = Concrete type constructor
 -
 - 1. (..)
 -
 - 2. f :: zed -> Zed -> Blah 
 -         FP     CT     CT
 -
 - 3. f :: Enum b => a -> b -> C 
 -                   FP   CP   CT 
 -
 - 4. f :: f -> g -> c
 -         FP   FP   CT 
 -
 -}

{- 
 - Write a type signature
 -}

-- 1
functionH :: [a] -> a
functionH (x:_) = x

-- 2
functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

-- 3
functionS :: (a, b) -> b
functionS (x, y) = y


{-
 - Given a type, write the function
 -}

-- 1
i :: a -> a
i a = a

-- 2
c :: a -> b -> a
c x y = x

-- 3
c'' :: b -> a -> b
c'' x y = x

-- 4
c' :: a -> b -> b
c' x y = y

-- 5
r :: [a] -> [a]
r = reverse 

-- 6
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC (aToB a)

-- 7 
a :: (a -> c) -> a -> a
a _ a = a

-- 8 
a' :: (a -> b) -> a -> b
a' aToB a = aToB a 


{-
 - Fix It
 -
 -}

-- 1

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x < y) then fstString x else sndString y
    where x = "Singin"
          y = "Somewhere"

-- 2 (..) Change '>' to '<'

-- 3
main :: IO()
main = do
  print $ 1 + 2 
  putStrLn "10"
  print (negate (-1))
  print ((+) 0 blah)
  where blah = negate 1

{-
 - Type-Kwon-Do
 -}

-- 1
f :: Int -> String 
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char 
h x = g (f x)

-- 2
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w (q x)

-- 3
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y) 

-- 4
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToY yToWCTuple x = fst (yToWCTuple (xToY x))

