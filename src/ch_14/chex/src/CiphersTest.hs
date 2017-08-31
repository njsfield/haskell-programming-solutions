module CiphersTest where

import Ciphers
import Test.QuickCheck
import Test.Hspec

onlyUpperCase :: String -> String
onlyUpperCase = filter (flip elem ['A'..'Z'])


-- 1 Casear Tests
caesarGen :: Gen (Int, String)
caesarGen = do
  a <- elements [1..26]
  b <- arbitrary
  return (a, onlyUpperCase b)

run_caesar :: IO()
run_caesar = do  
  putStrLn 
    "Caesar to Uncaesar \
     \ should hold for positive \
     \ ints (0-26) with capitalized sentences"
  quickCheck $ 
    forAll caesarGen
    (\(x,y) -> (uncaesar x (caesar x y)) == y)


main :: IO()
main = hspec $ do
  describe "Cipher Tests" $ do
    it "Ceasar Tests" $ do
      run_caesar
    it "Vigenere Tests" $ do
      vigenere "MEET AT DAWN" "ALLY" `shouldBe` "MPPR AE OYWY"
