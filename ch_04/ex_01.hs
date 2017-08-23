module ExerciseOne where

data Mood = Blah | Woot deriving Show

{-
 - Mood Swing
 - =========
 -
 - data Mood = Blah | Woot deriving Show
 -
 - 1. Mood is the type constructor 
 - 2. Blah | Woot are the values allowed
 - 3. type signature of changeMood should be 
 -  changeMood :: Mood -> Mood
 -}

-- 4

changeMood :: Mood -> Mood 
changeMood Blah = Woot
changeMood    _ = Blah


-- 5. (...)
