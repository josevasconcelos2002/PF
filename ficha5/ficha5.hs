module Ficha5 where

-- 3)

type Mat a = [[a]]

-- a)

dimOK :: Mat a -> Bool
dimOK [] = False
dimOK (((x:xs),(y:ys)):h) | length (x:xs) == length (y:ys) = True && dimOK h
                          | otherwise = False 