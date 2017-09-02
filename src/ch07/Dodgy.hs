module Ch07.Dodgy where

dodgy
  :: Num a
  => a -> a -> a
dodgy x y = x + y * 10

oneIsOne
  :: Num a
  => a -> a
oneIsOne = dodgy 1

oneIsTwo
  :: Num a
  => a -> a
oneIsTwo = (flip dodgy) 2

-- 1 (1)
a = dodgy 1 0

-- 2 (11)
b = dodgy 1 1

-- 3 (22)
c = dodgy 2 2

-- 4 (21) 
d = dodgy 1 2

-- 5 (12)
e = dodgy 2 1

-- 6 (11)
f = oneIsOne 1

-- 7 (21)
g = oneIsOne 2

-- 8 (21)
h = oneIsTwo 1

-- 9 (22)
i = oneIsTwo 2

-- 10 (31)
j = oneIsOne 3

-- 11 (23)
k = oneIsTwo 3
