module Ch26.Exercises where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Functor.Identity


-- 1)
rDec :: Num a => Reader a a
rDec = ReaderT $ Identity . flip (-) 1

-- 3)
rShow :: Show a => ReaderT a Identity String
rShow = reader show

-- 5)
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \i -> do
  print $ "Hi: " ++ show i
  return (i + 1)

-- 6)
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \i -> do
  print $ "Hi: " ++ show i
  return (show i, i + 1)


-- Fixes

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e  -> putStrLn ("Good, was very excite: " ++ e)
