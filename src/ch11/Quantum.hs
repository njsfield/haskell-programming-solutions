module Ch11.Quantum where

{-
 - convert :: Quantum -> Bool
 -
 - Should have 8 implementations
 -
 - convert Yes  = True
 - convert No   = True
 - convert Both = False
 -
 - convert Yes  = False
 - convert No   = False
 - convert Both = True
 -
 - convert Yes  = False
 - convert No   = True
 - convert Both = False
 -
 - convert Yes  = True
 - convert No   = False
 - convert Both = True
 -
 - convert Yes  = True
 - convert No   = True
 - convert Both = True
 -
 - convert Yes  = False
 - convert No   = False
 - convert Both = False
 -
 - 18 possible implementations found
 -}


{-
 - The Quad
 -}

data Quad =
    One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

-- 1
eQuad :: Either Quad Quad
eQuad = undefined

-- Sum Type: Can have 4 + 4 (8) variations (because of Either,
-- which can be treated like a Product type with 2 * 4)

-- 2
prodQuad :: (Quad, Quad)
prodQuad = undefined

-- Product Type: Can have 4 * 4 (16) variations

-- 3
funcQuad :: Quad -> Quad
funcQuad = undefined

-- Function Type: Can have 4 ^ 4 (256) variations (exponential laws of cardinality)

-- 4
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined

-- Product Type: Can have 2 * 2 * 2 (8) variations

-- 5
gTwo :: Bool -> Bool -> Bool
gTwo = undefined

-- Function Type: Can have 2 ^ (2 * 2) (16) variations (using mathmatical conversion for ^2 issues)

-- 6
fTwo :: Bool -> Quad -> Quad
fTwo = undefined

-- Function Tuple: Can have 4 ^ (4 * 2) (65536) variations

