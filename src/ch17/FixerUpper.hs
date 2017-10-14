module Ch17.FixerUpper where

-- 1
a = const <$> Just "Hello" <*> Just "World"

-- 2
b = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1,2,3]
