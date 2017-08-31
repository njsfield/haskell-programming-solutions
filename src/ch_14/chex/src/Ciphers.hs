module Ciphers where

import Data.Bool
import Data.Char

{-
 - Caesar
 -}

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

{-
 - Vigenere
 -}

type Keyword = String
type Message = String

type KeywordHead = Char
type MessageHead = Char

-- Transpose a char by the other chars A-Z mapping to 0-26
-- (works with Capitals only)
transposed :: MessageHead -> KeywordHead -> MessageHead
transposed mh kh = chr
                 . flip (+) 65
                 . flip rem 26
                 . flip (-) 65
                 . (+) (ord kh - 65)
                 . ord
                 $ mh

vigenere :: Message -> Keyword -> Message
vigenere [] _                = []
vigenere m []                = m
vigenere (' ':ms) k        = ' '  : vigenere ms k
vigenere (mh:ms)  (kh:ks) = res : vigenere ms (ks ++ [kh])
    where res = transposed (toUpper mh) (toUpper kh)

-- @TODO Add unvigenere
