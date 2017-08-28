module ExerciseSeven where

{-
 - How Does Your Garden Grow
 -}

type Gardener = String
-- In Normal Form

data Garden =
    Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show
