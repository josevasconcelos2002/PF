module FICHA1 where

import Data.Char
import Data.List


-- 1 a)

perimetro :: Float -> Float
perimetro r = r*2*3.1416

-- 1 b)

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt((x1-x2)^2 + (y1-y2)^2)

-- 1 c)

primUlt :: [Double] -> (Double,Double)
primUlt l = (head l, last l)

-- 1 d)

multiplo :: Int -> Int -> Bool

multiplo a b | (mod a b) == 0 = True
             | otherwise = False 

-- 1 e)

truncaImpar :: [Double] -> [Double]
truncaImpar [] = []
truncaImpar (x:xs) | (mod (length (x:xs)) 2) == 0  = (x:xs)
                   | otherwise = xs

-- 1 f)

max2 :: Int -> Int -> Int
max2 a b | a >= b = a
         | otherwise = b

-- 1 g)

max3 :: Int -> Int -> Int -> Int
max3 a b c | c >= max2 a b = c
           | otherwise = max2 a b




-- 3

type Hora = (Int,Int)

-- a)

testa_h :: Hora -> Bool
testa_h (a,b) | a >= 00 && a <= 23 && b >= 00 && b <= 59 = True
              | otherwise = False 

-- b)

compara_h :: Hora -> Hora -> Bool
compara_h (a,b) (c,d) |  testa_h (a,b) == True && testa_h (c,d) == True && a > c = True
                      |  testa_h (a,b) == True && testa_h (c,d) == True && a == c && b > d = True
                      |  otherwise = False 

-- c)

converte_h_m :: Hora -> Int
converte_h_m (a,b) | testa_h (a,b) == True = a*60 + b
                   | otherwise = 0

-- d)

converte_m_h :: Int -> Hora
converte_m_h x | x >= 0 && x <= 1439 = (div x 60 , mod x 60)
               | otherwise = (0,0)

-- e) 

diferença :: Hora -> Hora -> Int
diferença (a,b) (c,d) | converte_h_m (a,b) >= converte_h_m (c,d) = converte_h_m (a,b) - converte_h_m (c,d)
                      | converte_h_m (c,d) > converte_h_m (a,b) = converte_h_m (c,d) - converte_h_m (a,b)
                      | otherwise = 0

-- f)

adiciona :: Hora -> Int -> Hora
adiciona (a,b) x = converte_m_h (converte_h_m (a,b) + x) 


-- 4

data Hora1 = H Int Int 
     deriving (Show,Eq)

-- a)

testa_h1 :: Hora1 -> Bool
testa_h1 (H a b)    | a >= 00 && a <= 23 && b >= 00 && b <= 59 = True
                    | otherwise = False 


-- b)

compara_h1 :: Hora1 -> Hora1 -> Bool
compara_h1 (H a b) (H c d) |  testa_h1 (H a b) == True && testa_h1 (H c d) == True && a > c = True
                           |  testa_h1 (H a b) == True && testa_h1 (H c d) == True && a == c && b > d = True
                           |  otherwise = False

-- c)

converte_h_m1 :: Hora1 -> Int
converte_h_m1 (H a b) | testa_h1 (H a b) == True = a*60 + b
                      | otherwise = 0

-- d)

converte_m_h1 :: Int -> Hora1
converte_m_h1 x | x >= 0 && x <= 1439 = (H (div x 60)  (mod x 60))
                | otherwise = (H 0 0)

-- e) 

diferença1 :: Hora1 -> Hora1 -> Int
diferença1 (H a b) (H c d) | converte_h_m1 (H a b) >= converte_h_m1 (H c d) = converte_h_m1 (H a b) - converte_h_m1 (H c d)
                           | converte_h_m1 (H c d) > converte_h_m1 (H a b) = converte_h_m1 (H c d) - converte_h_m1 (H a b)
                           | otherwise = 0

-- f)

adiciona1 :: Hora1 -> Int -> Hora1
adiciona1 (H a b) x = converte_m_h1 (converte_h_m1 (H a b) + x)


-- 5

data Semaforo = Verde | Amarelo | Vermelho 
            deriving (Show,Eq)


-- a)

next :: Semaforo -> Semaforo
next a | a == Verde = Amarelo 
       | a == Amarelo = Vermelho
       | otherwise = Verde

-- b)

stop :: Semaforo -> Bool
stop a | a == Vermelho = True
       | otherwise = False

-- c)

safe :: Semaforo -> Semaforo -> Bool
safe a b | a == Vermelho && b == Verde = True
         | a == Verde && b == Vermelho = True
         | otherwise = False



-- 8

-- a)


my_isLower :: Char -> Bool
my_isLower a | ord a >= 97 && ord a <= 122 = True
             | otherwise = False


n_lower :: [Char] -> Int        -- n de caracteres minusculos numa palavra
n_lower (x:xs) | ord x >= 97 && ord x <= 122 = 1 + n_lower xs
               | otherwise = n_lower xs


-- b)



-- c)


isAlpha1 :: Char -> Bool
isAlpha1 a | ord a >= 65 && ord a <= 90 || ord a >= 97 && ord a <= 122 = True
           | otherwise = False 


-- d)


toUpper1 :: Char -> Char
toUpper1 a | a == 'a' = 'A'
           | a == 'b' = 'B'
           | a == 'c' = 'C'
           | a == 'd' = 'D'
           | a == 'e' = 'E'
           | a == 'f' = 'F'
           | a == 'g' = 'G'
           | a == 'h' = 'H'
           | a == 'i' = 'I'
           | a == 'j' = 'J'
           | a == 'k' = 'K'
           | a == 'l' = 'L'
           | a == 'm' = 'M'
           | a == 'n' = 'N'
           | a == 'o' = 'O'
           | a == 'p' = 'P'
           | a == 'q' = 'Q'
           | a == 'r' = 'R'
           | a == 's' = 'S'
           | a == 't' = 'T'
           | a == 'u' = 'U'
           | a == 'v' = 'V'
           | a == 'w' = 'W'
           | a == 'x' = 'X'
           | a == 'y' = 'Y'
           | a == 'z' = 'Z'

           
-- e)


{-
intToDigit1 :: Int -> Char
intToDigit1 a | ord a >= 48 && ord a <= 57 = chr(ord a)
              | otherwise = 'n'
-}


intToDigit2 :: Int -> Char
intToDigit2 a | a == 0 = '0'
              | a == 1 = '1'
              | a == 2 = '2'
              | a == 3 = '3'
              | a == 4 = '4'
              | a == 5 = '5'
              | a == 6 = '6'
              | a == 7 = '7'
              | a == 8 = '8'
              | a == 9 = '9'



-- f)


digitToInt1 :: Char -> Int
digitToInt1 a | a == '0' = 0
              | a == '1' = 1
              | a == '2' = 2
              | a == '3' = 3
              | a == '4' = 4
              | a == '5' = 5
              | a == '6' = 6
              | a == '7' = 7
              | a == '8' = 8
              | a == '9' = 9