module Aula1 where

--1)

--a)
perimetro :: Float -> Float
perimetro r = 3.1416 * r

--b)
dist :: (Double,Double) -> (Double,Double) -> Double
dist (a,b) (c,d) = sqrt((a-c)*(a-c) + (b-d)*(b-d))

--c)
primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

--d)
multiplo :: Int -> Int -> Bool
multiplo a b | mod b a == 0 = True
             | otherwise = False

--e)
truncaImpar :: [a] -> [a]
truncaImpar l | mod (length l) 2 == 0 = l
              | otherwise = tail l

--f)
max2 :: Int -> Int -> Int
max2 a b | a >= b = a
         | otherwise = b 

--g)
max3 :: Int -> Int -> Int -> Int
max3 a b c | (max2 a b) >= c = (max2 a b)
           | otherwise = c

--3)
type Hora = (Int,Int)

--a)
valida :: Hora -> Bool
valida (a,b) | a >= 0 && a<24 && b >= 0 && b<60 = True
             | otherwise = False

--b)
depois :: Hora -> Hora -> Bool
depois (a,b) (c,d) | valida (a,b) && valida (c,d) && a>c = True
                   | valida (a,b) && valida (c,d) && a == c && b>d = True
                   | otherwise = False

--c)
converte_h_m :: Hora -> Int
converte_h_m (a,b) | valida(a,b) = a*60 + b 
                   | otherwise = 0

--d)
converte_m_h :: Int -> Hora
converte_m_h x = (div x 60,mod x 60)

--e)
diferença :: Hora -> Hora -> Int
diferença (a,b) (c,d) | valida(a,b) && valida(c,d) && converte_h_m (a,b)>=converte_h_m(c,d) =converte_h_m(a,b) - converte_h_m(c,d)
                      | otherwise = 0

--f)
adiciona :: Int -> Hora -> Hora
adiciona x (a,b) | valida(a,b) = (a+div x 60,b+mod x 60)
                 | otherwise = (0,0)

--5)
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

--a)
next :: Semaforo -> Semaforo
next a | a == Verde = Amarelo
       | a == Amarelo = Vermelho
       | otherwise = Verde 

--b)
stop :: Semaforo -> Bool
stop a | a == Vermelho = True
       | otherwise = False

--c)
safe :: Semaforo -> Semaforo -> Bool
safe a b | a == Verde && b == Verde = False
         | otherwise = True 

--6)
data Ponto = Cartesiano Double Double | Polar Double Double
             deriving (Show,Eq)
{-
--a)
posx :: Ponto -> Double
posx 
}