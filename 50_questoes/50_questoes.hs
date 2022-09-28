module Questoes where


--1)
enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 a b | a==b = [a]
                | b>a = [a..b]
                | otherwise = []