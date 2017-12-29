module Ch24.Exercises where

import Text.Trifecta

-- force fail
stop :: Parser ()
stop = unexpected "stop"

-- parse '1'
one :: Parser Char
one = char '1'

-- parse '1' & '2' chars
oneTwo :: Parser Char
oneTwo = one >> char '2'

-- parse '1' then succeed if stream finished
oneEof :: Parser ()
oneEof = one >> eof

-- parse '1' & '2' then succeed if stream finished
oneTwoEof :: Parser ()
oneTwoEof = oneTwo >> eof

-- lift show over integer parser
intString :: Parser String
intString = show <$> integer

-- Parse some/all of string
stringParser :: String -> Parser String
stringParser s = go s mempty
  where
    go (x:xs) parsed = char x >>= (\x' -> go xs (parsed ++ [x']))
    go [] parsed     = return parsed

-- Parse int followed by eof
intParser :: Parser Integer
intParser = do
  i <- integer
  eof >>= const (return i)

-- Parse single digit (as char)
parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

-- Parse pos/negative integer
base10Integer :: Parser Integer
base10Integer = do
  s <- option ' ' (char '-')
  (read . (++) [s] <$> some parseDigit) <?> "integer"

main = do
  print $ parseString stop mempty "123"
  print $ parseString one mempty "123"
  print $ parseString oneTwo mempty "123"
  print $ parseString oneTwoEof mempty "123"
  print $ parseString intString mempty "123"
  print $ parseString (stringParser "1") mempty "123"
  print $ parseString (stringParser "12") mempty "123"
  print $ parseString (stringParser "123") mempty "123"
  print $ parseString intParser mempty "123"
  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc"
  print $ parseString base10Integer mempty "123abc"
  print $ parseString base10Integer mempty "-123abc"
  print $ parseString base10Integer mempty "abc"
