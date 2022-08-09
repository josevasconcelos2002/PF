module Ficha2 where

import Data.Char

-- 2

-- a)

dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = 2*x : dobros xs

-- b)

numOcorre :: Char -> String -> Int
numOcorre ' ' _ = 0
numOcorre a (x:xs) | a == x = 1 + numOcorre a xs
                   | otherwise = numOcorre a xs

-- c)

positivos :: [Int] -> Bool
positivos [] = False
positivos (x:xs) | x < 0 = False
                 | otherwise = True && positivos xs

-- d)

soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs) | x < 0 = soPos xs
             | otherwise = x : soPos xs

-- e)

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:xs) | x < 0 = x + somaNeg xs
               | otherwise = somaNeg xs


-- f)
{-
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt l | length l < 4 = l
          | otherwise =                  -- ?
-}

-- g)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,xs):h) = xs : segundos h 


-- h)

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros a ((x,xs):h) | a == x = True
                          | otherwise = False || nosPrimeiros a h


-- i)

{-
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos ((a,b,c),(d,e,f)):h = (a+d,b+e,c+f):sumTriplos h
-} 

-- 3

-- a)

{-
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs) | ord x >=  && ord x <=  = x : soDigitos xs
                 | otherwise = soDigitos xs
-}

-- b)

-- 4
type 
-- a)

conta :: Int ->