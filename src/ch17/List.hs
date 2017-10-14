module Ch17.List where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- List

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

{-
 - e.g.
 - append (Cons 1 (Cons 2 Nil))   (Cons 3 Nil)
 - Cons 1 $ (Cons 2 Nil) `append` (Cons 3 Nil)
 -          append (Cons 2 Nil)   (Cons 3 Nil)
 -          Cons 2 $ Nil `append  (Cons 3 Nil)
 -                   append   Nil (Cons 3 Nil)
 -                                (Cons 3 Nil)
 - =
 - Cons 1 (Cons 2 (Cons 3 Nil))
 -
 --}

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

take' :: Int -> List a -> List a
take' 0 _          = Nil
take' n (Cons h t) = Cons h (take' (n - 1) t)

-- Instances

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Nil
  Nil <*> _   = Nil
  _   <*> Nil = Nil
  (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)

genList :: Arbitrary a => Gen (List a)
genList = do
  h <- arbitrary
  t <- genList
  frequency [ (1, return Nil)
            , (2, return (Cons h t)) ]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

instance Eq a => EqProp (List a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let l = xs
                    in take' 3000 l
              ys' = let l = ys
                    in take' 3000 l

-- ZipList

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (Cons a Nil)
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ (zipListWith fs xs)

zipListWith :: List (a -> b) -> List a -> List b
zipListWith _ Nil                    = Nil
zipListWith Nil _                    = Nil
zipListWith (Cons f Nil) (Cons x xs) = Cons (f x) (pure f <*> xs)
zipListWith (Cons f fs) (Cons x Nil) = Cons (f x) (fs <*> pure x)
zipListWith (Cons f fs) (Cons x xs)  = Cons (f x) (zipListWith fs xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l
listSpec :: IO ()
listSpec = do
  quickBatch $ applicative (Cons ("a", "b", "c") Nil)
  quickBatch $ applicative (ZipList' (Cons ("a", "b", "b") Nil))



