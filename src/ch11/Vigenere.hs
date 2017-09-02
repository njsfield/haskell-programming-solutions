module Ch11.Vigenere where

import Data.Char

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
vigenere [] _                 = []
vigenere m@(' ':ms) k         = ' ' : vigenere ms k
vigenere m@(mh:ms)  k@(kh:ks) = res : vigenere ms (ks ++ [kh])
    where res = transposed (toUpper mh) (toUpper kh)

