module ExerciseEight where

{-
 - Zipping exercises
 -}

-- 1
myZip :: [a] -> [b] -> [(a,b)]
myZip [] _          = []
myZip _ []          = []
myZip (h:t) (h':t') = [(h,h')] ++ (myZip t t')

-- 2
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _          = []
myZipWith _ _ []          = []
myZipWith f (h:t) (h':t') = [f h h'] ++ (myZipWith f t t')
