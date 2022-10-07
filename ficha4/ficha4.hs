module Ficha4 where

import Data.Char 

 -- exercicio 1
digitAlpha :: String -> (String,String)
digitAlpha s = digitAlpha_aux s ("","")

    where digitAlpha_aux :: String -> ([Char],[Char]) -> (String,String)
          digitAlpha_aux [] (a,b) = (a,b)
          digitAlpha_aux (x:xs) (a,b) | (65<=ord x && ord x <=90) || (97<=ord x && ord x <= 122)= digitAlpha_aux xs (a++[x],b)
                                      | (48 <= ord x && ord x <= 57)  = digitAlpha_aux xs (a,b++[x])   



-- exercicio 2

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp l = nzp_aux l (0,0,0)

  where nzp_aux :: [Int] -> (Int,Int,Int) -> (Int,Int,Int)
        nzp_aux [] (a,b,c) = (a,b,c)
        nzp_aux (x:xs) (a,b,c) | x<0 = nzp_aux xs (a+1,b,c)
                               | x==0 = nzp_aux xs (a,b+1,c)
                               | otherwise = nzp_aux xs (a,b,c+1) 

-- exercicio 3
{-
3. Defina a fun¸c˜ao divMod :: Integral a => a -> a -> (a, a)
que calcula simultaneamente a divis˜ao e o resto da divis˜ao inteira por subtrac¸c˜oes sucessivas.
-}
divMod1 :: Integral a => a -> a -> (a,a)
divMod1 a b = divMod_aux a b (0,0)

   where divMod_aux :: Integral a => a -> a -> (a,a) -> (a,a)
         divMod_aux 0 0 (q,r) = (q,r)
         divMod_aux a b (q,r) | a == b = divMod_aux 0 0 (q+1,r)
                              | a > b = divMod_aux (a-b) b (q+1,r)
                              | otherwise = divMod_aux 0 0 (q,r+a)

-- exercicio 4
{-
Utilizando uma fun¸c˜ao auxiliar com um acumulador, optimize seguinte defini¸c˜ao recursiva que determina qual o n´umero que corresponde a uma lista de digitos.
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t
Note que
fromDigits [1,2,3,4] = 1 × 103 + 2 × 102 + 3 × 101 + 4 × 100
= 4 + 10 × (3 + 10 × (2 + 10 × (1 + 10 × 0)))
-}
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits l = fromDigits_aux l 0

     where fromDigits_aux :: [Int]-> Int -> Int
           fromDigits_aux [] ac = ac 
           fromDigits_aux (x:xs) ac = fromDigits_aux xs (x*10^(length (xs))+ac) 

-- exercicio 7
{-
intToStr :: Integer -> String
intToStr l = intToStr_aux l ""

    where intToStr_aux :: Integer -> [Char] -> String
          intToStr_aux 0 s = s
          intToStr_aux (x:xs) s = intToStr_aux xs (s++)
-}

-- exercicio 9

--a)
-- [2**a|a<-[0..10]]

--b)
-- [(a,b) |a<-[1..5],b<-[5,4,3,2,1]]