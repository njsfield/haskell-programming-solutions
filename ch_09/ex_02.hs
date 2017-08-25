module ExerciseTwo where

-- 1
myWordsAt :: Char -> String -> [String]
myWordsAt _ []      = []
myWordsAt x s@(h:t) = res
  where res  = if x == h then skip else exec
        skip = myWordsAt x t  
        exec = [h'] ++ (myWordsAt x t') 
        h'   = takeWhile (/= x) s
        t'   = dropWhile (/= x) s

myWords :: String -> [String]
myWords = myWordsAt ' '

-- 2

firstSen  = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen  = "What immortal hand of eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = myWordsAt '\n'

shouldEqual = 
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand of eye" 
  , "Could frame thy fearful symmetry?"
  ]

main :: IO()
main = 
  print $ "Are they equal? "
          ++ show (myLines sentences == shouldEqual)
