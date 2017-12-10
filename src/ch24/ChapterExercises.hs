{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

module Ch23.ChapterExercises where

import           Control.Arrow (first)

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi (first f . g)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (a, )

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b

-- Moi f = Moi (s -> (a -> b, s))
-- Moi g = Moi (s -> (a, s))
  Moi f <*> Moi g = Moi (\s ->
                        let (ab, s') = f s
                            (a, s'') = g s'
                        in  (ab a, s''))

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g = Moi (\s ->
                  let (a, s') = f s
                  in runMoi (g a) s')

-- 1)
get :: Moi s s
get = Moi (\s -> (s,s))

-- 2)
put :: s -> Moi s ()
put s = Moi (\s -> ((),s))

-- 3)
exec :: Moi s a -> s -> s
exec (Moi sa) s = snd (sa s)

-- 4)
eval :: Moi s a -> s -> a
eval (Moi sa) s = fst (sa s)

-- 5)
modify :: (s -> s) -> Moi s ()
modify f = Moi (\s -> ((), f s))
