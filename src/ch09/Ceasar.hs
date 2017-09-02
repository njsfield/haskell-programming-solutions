module Ch09.Caesar where

import Data.Bool
import Data.Char

caesarShiftRight :: Int -> String -> String
caesarShiftRight _ []      = []
caesarShiftRight x (s:t)   = [shifted x s] ++ caesarShiftRight x t
  where shifted x' s' = chr
                      . flip (+) minOrd
                      . flip rem 26
                      . flip (-) minOrd
                      . (+) x'
                      . ord
                      $ s'
        minOrd = bool 97 65 (isUpper s)


caesarShiftRightOne :: String -> String
caesarShiftRightOne = caesarShiftRight 1

caesar :: Int -> String -> String
caesar i s = caesarShiftRight (abs i) s

-- Reverse
uncaesar :: Int -> String -> String
uncaesar i s = caesar (26 - i) s
