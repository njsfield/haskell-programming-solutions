module Ch24.IPAddress where

import           Data.Char     (intToDigit)
import           Data.Word
import           Numeric       (showHex, showIntAtBase)
import           Text.Trifecta

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

ipAddressParser :: Parser IPAddress
ipAddressParser = do
  a <- (* 256^3) <$> integer
  _ <- char '.'
  b <- (* 256^2) <$> integer
  _ <- char '.'
  c <- (* 256) <$> integer
  _ <- char '.'
  d <- integer
  eof
  return $ IPAddress . fromIntegral $ sum [a,b,c,d]

main :: IO ()
main = do
  print $ parseString ipAddressParser mempty "172.16.254.1"
