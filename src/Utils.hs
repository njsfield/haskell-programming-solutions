module Utils where

-- elm style forwards pipe operator
-- credit to @andrewMacmurray
(|>) :: a -> (a -> b) -> b
x |> f = f x
