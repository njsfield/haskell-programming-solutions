module Ch15.Monoids where

import           Ch15.QuickChecking
import           Ch15.Semigroups
import           Data.Semigroup
import           Test.Hspec
import           Test.QuickCheck

-- 1
instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

-- 2
instance (Monoid a, Semigroup a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

-- 3
instance ( Monoid a
         , Semigroup a
         , Monoid b
         , Semigroup b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

-- 4
instance Monoid BoolConj where
  mempty = BoolConj False
  mappend = (<>)

-- 5
instance Monoid BoolDisj where
  mempty = BoolDisj True
  mappend = (<>)

-- 6
instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

-- 7
instance (Monoid a, Semigroup a) => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)

-- 8
newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

composeTup :: Semigroup a
           => (s -> (a, s))
           -> (s -> (a, s))
           -> s
           -> (a, s)
composeTup f g x =
  let
    (a, b) = g x
    (c, d) = f b
  in
    (a <> c, d)

instance (Semigroup a) => Semigroup (Mem s a) where
  Mem { runMem = f } <> Mem { runMem = g } = Mem { runMem = \x -> composeTup f g x }

instance (Monoid a, Semigroup a) => Monoid (Mem s a) where
  mempty = Mem $ (,) mempty -- expect s
  mappend = (<>)

-- f' :: Mem { runMem :: String -> (Int, String) }
-- f' =  Mem $ \s -> ("hi", s + 1)
--
-- res :: (String, Int)
-- res =  runMem (f' <> mempty) 0
--
-- runMem (Mem $ \s -> ("hi", s + 1) <> \s -> (mempty, s)) 0
-- runMem (Mem $ \s -> ("hi", s + 1) <> (mempty, 0))
-- runMem (Mem $ \s -> ("hi", s + 1) <> ("", 0))
-- runMem (Mem $ ("hi", 1) <> ("", 0))
-- "h1" <> "" == "h1"
-- ("h1", 1)
--
-- res =  runMem (mempty <> f') 0
--
-- runMem (Mem $ \s -> (mempty, s) <> \s -> ("hi", s + 1)) 0
-- runMem (Mem $ \s -> (mempty, s) <> ("hi", 1))
-- runMem (Mem $ \s -> ("", s) <> ("hi", 1))
-- runMem (Mem $ ("", 1) <> ("hi", 1))
--
-- "" <> "h1"
-- ("h1", 1)


main :: IO ()
main = do
  quickCheck (monoidAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  quickCheck (monoidAssoc :: IdentityAssocString)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

  quickCheck (monoidAssoc :: TwoAssocString)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)

  quickCheck (monoidAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  quickCheck (monoidAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  hspec $ do
    it "Mem tests" $ do
      let f' = Mem $ \x -> ("Hi", x + 1)

      runMem (f' <> mempty) 0 `shouldBe` ("Hi", 1)
      runMem (mempty <> f') 0 `shouldBe` ("Hi", 1)
      (runMem mempty 0 :: (String, Int)) `shouldBe` ("", 0)
      runMem (f' <> mempty) 0 == runMem f' 0 `shouldBe` True
      runMem (mempty <> f') 0 == runMem f' 0 `shouldBe` True
