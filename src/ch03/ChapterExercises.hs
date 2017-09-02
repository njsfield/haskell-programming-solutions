module Ch03.ChapterExercises where
 {- Building functions
 - ==================
 - 
 - Using -
 -  take :: Int -> [a] -> [a]
 -  drop :: Int -> [a] -> [a]
 -
 -}

{-
 - Reading syntax
 - ==============
 -
 - 1.
 -
 - a) concat [[1, 2, 3], [4, 5, 6]] (correct)
 - b)  ++  [1, 2, 3] [4, 5, 6]      (incorrect)  
 -    (++) [1, 2, 3] [4, 5, 6]      (correct) 
 -
 - c) (++) "hello" " world"         (correct)
 - d) ["hello" ++ " world]          (incorrect)
 -    ["hello" ++ " world"]
 -
 - e) 4 !! "hello"                  (incorrect)
 -    "hello" !! 4                  (correct)
 -
 - f) (!!) "hello" 4                (correct)
 - g) take "4 lovely"               (incorrect)
 -    take 4 "lovely"               (correct)
 -
 - h) take 3 "awesome"              (correct)
 -
 -
 - 2. 
 -
 - a) concat [[1 * 6], [2 * 6], [3 * 6]]    == [6, 12, 18]
 - b) "rain" ++ drop 2 "elbow"              == "rainbow"
 - c) 10 * head [1, 2, 3]                   == 10
 - d) (take 3 "Julie") ++ (tail "yes")      == "Jules"
 - e) concat [tail [1, 2, 3],               
 -            tail [4, 5, 6],
 -            tail [7, 8, 9]]               == [2, 3, 5, 6, 8, 9]
 -}
-- 1
-- a) exclaim "Hello World" == "Hello World!"
exclaim x = x ++ "!"

-- b) getFifth "Curry is awesome!" == "y" 
getFifth x = drop 4 (take 5 x)

-- c) dropNine "Curry is awesome!" == "awesome!"
dropNine = drop 9

-- 2 (...)
-- 3 
thirdLetter :: String -> Char
thirdLetter x = x !! 3

-- 4 
letterIndex :: Int -> Char
letterIndex = (!!) "Curry is awesome"

-- 5.
rvrs x = (dropNine x) ++ (take 4 (drop 5 x)) ++ (take 5 x)
-- 6. (See reverse.hs)
