module ChapterExercises where

{-
 - Parenthesization
 - ===============
 -
 - Precedences
 - (+) - 6
 - (-) - 6
 - (*) - 7
 - (^) - 8
 -
 - 1. 2 + 2 * 3 - 1  
 -  = (2 + (2 * 3)) - 1
 -
 - 2. (^) 10 $ 1 + 1 
 -  = (^) 10 (1 + 1)
 -
 - 3. 2 ^ 2 * 4 ^ 5 + 1
 -  = ((2 ^ 2) * (4 ^ 5)) + 1
 -
 - Equivalent expressions
 - ======================
 -
 - 1. 1 + 1       == 2            (true)
 - 2. 10 ^ 2      == 10 + 9 * 10  (true)
 - 3. 400 - 37    == (-) 37 400   (false)
 - 4. 100 `div` 3 == 100 / 3      (false - div is for Integral types, (/) is for fractional) 
 - 5. 2 * 5 + 18  == 2 * (5 + 18) (false due to precendence)
 -
 - -}


{-
 - More fun with functions
 - ======================
 -}

z = 7 
x = y ^ 2     --225
waxOn = x * 5 --1125
y = z + 8     --15

{-
 - 1. 10 + waxOn   == 1135
 -    (+10) waxOn  == 1135
 -    (-) 15 waxOn == -1100
 -    (-) waxOn 15 == 1100
 -
 - 2. (enter let triple x = x * 3) at prompt
 - 
 - 3. triple waxOn == 3375 (both REPL functions & module functions allowed in REPL)
 -
 -}

-- 4. (local variables)
waxOn'    = x * 5
  where x = y ^ 2 
        y = z + 8
        z = 7

-- 5.
triple = (*) 3


-- 6.
waxOff x = triple x
