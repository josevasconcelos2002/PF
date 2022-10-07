module Ficha5 where

-- exercicio 1

--a)
any1 :: (a -> Bool) -> [a] -> Bool
any1 _ [] = False
any1 f (x:xs) | f x == True = True
              | otherwise = any1 f xs 

--b)
zipWith1 :: (a->b->c) -> [a] -> [b] -> [c]
zipWith1 _ [] [] = []
zipWith1 f (x:xs) (y:ys) = (f x y) : zipWith1 f xs ys

--c)
takeWhile1 :: (a->Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 f (x:xs) | f x == True = x : takeWhile1 f xs
                    | otherwise = []