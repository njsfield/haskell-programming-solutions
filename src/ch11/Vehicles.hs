module Ch11.Vehicles where

data Price =
    Price Integer
    deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
    deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
  | Plane Integer Airline
    deriving (Eq, Show)

{-
 - Vehicles
 -}

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane (1000) PapuAir

-- 1. myCar :: Vehicle

-- 2.
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar


-- 3)
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

-- 4) If getManu is applied to a Vehicle with Plane data constructor, result will be ⊥

-- 5) (See additions)


