module Questoes where


--1)
enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 a b | a==b = [a]
                | b>a = [a..b]
                | otherwise = []

--2)
{-
enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 a b c | a == c = [a]
                      | a > c = []
                      | 
-}

--3)
concatena :: [a] -> [a] -> [a]
concatena [] [] = []
concatena [] l = l
concatena l [] = l 
concatena a b = a ++ b

--4)
{-
indice :: [a] -> Int -> a 
indice (x:xs) n | n == 0 = x
                | otherwise = indice xs n
-}

--5)
inverte :: [a] -> [a]
inverte [] = []
inverte (x:xs) = (inverte xs)++[x]

--6)
take1 :: Int -> [Int] -> [Int]
take1 _ [] = []
take1 0 l = l
take1 n (x:xs) | n>length(x:xs) = []
               | n == length(x:xs) = (x:xs)
               | otherwise = (x : take1 (n-1) xs )

--7)
drop1 :: Int -> [Int] -> [Int]
drop1 _ [] = []
drop1 0 l = l
drop1 n (x:xs) | n>=length(x:xs) = [] 
               | otherwise = drop1 (n-1) xs

--8)
zip1 :: [a] -> [b] -> [(a,b)]
zip1 [] [] = []
zip1 (x:xs) (y:ys) | length(x:xs) == length(y:ys) = ((x,y)) : zip1 xs ys
                   | length(x:xs) > length(y:ys) = ((x,y)) : zip1 (take (length(x:xs)-length(y:ys)) (xs)) ys
                   | otherwise = ((x,y)) : zip1  xs (take (length(y:ys)-length(x:xs)) (y:ys))

--9)
replicar :: Int -> a -> [a]
replicar 0 _ = []
replicar n a = [a] ++ replicar (n-1) a

--10)
intersperce1 :: a -> [a] -> [a]
intersperce1 _ [] = []
intersperce1 x (y:ys) = [y] ++ [x] ++ intersperce1 x ys

--11)
group1 :: Eq a => [a] -> [[a]]
group1 [] = []
group1 [x] = [[x]]                  -- incompleta
group1 (x:h:xs) | x == h = [[x]++[h]] ++ group1 (h:xs)
                | otherwise = [[x]] ++ group1 (h:xs)

--12)
concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 ((x:xs):h) | length(x:xs) == 1 = [x] ++ concat1 h
                   | otherwise = (x:xs) ++ concat1 h

--13)
--incompleto
inits1 :: [a] -> [[a]]
inits1 [] = [[]]
inits1 (x:xs) = [take 0 (x:xs)] ++ [take 1 (x:xs)] ++ inits1 xs 

--14)




--15)
heads1 :: [[a]] -> [a]
heads1 [[]] = []
heads1 ((x:xs):h) = [x] ++ heads1 h

--16)
total :: [[a]] -> Int
total [[]] = 0
total ((x:xs):h) = length(x:xs) + total h

--17)
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):h) = ((a,c):fun h)

--18)
cola :: [(String,b,c)] -> String
cola [] = " "
cola ((s,a,b):h) = s ++ cola h

--19)
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade a i ((n,d_n):h) | (a-d_n) >= i = [n] ++ (idade a i h)
                      | otherwise = (idade a i h)

--20)
{-
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom _ 1 = [1]
powerEnumFrom 0 _ = undefined
powerEnumFrom b e | [b*b*0] ++ [b*b*1] ++ powerEnumFrom b
-}

--29)
{-
union1 :: Eq a => [a] -> [a] -> [a]
union1 [] [] = []
union1 l [] = l
union1 [] l = l
union1 (x:xs) (y:ys) | y == x = (x:xs) ++ (union1 (x:xs) ys)
                     | 
-}

--26)
--incompleta
nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (x:h:xs) | h == x = [x] ++ nub1 (h:xs)
              | otherwise = [x] ++ [h] ++ nub1 (h:xs)

--31)
insert1 :: Ord a => a -> [a] -> [a]
insert1 a [] = [a]
insert1 a (x:xs) | a<=x = (a:x:xs)
                 | otherwise = x:(insert1 a xs)
            


--32)
unwords1 :: [String] -> String
unwords1 [] = " "
unwords1 (x:h:xs) = x ++ " " ++ h ++ unwords1 xs

--33)
unlines1 :: [String] -> String
unlines1 [] = " "
unlines1 (x:xs) = x ++ "\n" ++ unlines1 xs

--46)
data Movimento = Norte | Sul | Este | Oeste
               deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (a,b) (c,d) | a==c && b==d = []
                    | a==c && b>d  = [Sul] ++ caminho (a,b-1) (c,d)
                    | a==c && b<d  = [Norte] ++ caminho (a,b+1) (c,d)
                    | a>c && b==d  = [Oeste] ++ caminho (a-1,b) (c,d)
                    | a<c && b==d  = [Este] ++ caminho (a+1,b) (c,d)
                    | a>c && b>d  = [Oeste,Sul] ++ caminho (a-1,b-1) (c,d)
                    | a>c && b<d  = [Oeste,Norte] ++ caminho (a-1,b+1) (c,d)
                    | a<c && b>d  = [Este,Sul] ++ caminho (a+1,b-1) (c,d)
                    | a<c && b<d  = [Este,Norte] ++ caminho (a+1,b+1) (c,d)



--47)
{-
hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops (a,b) (x:xs) | x == Norte = 
-}




--48)
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto




--49)
-- ver melhor , por causa da fórmula da area do retangulo, de acordo com a sua hipotenusa
{-
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0.0
areaTotal [x] = 
areaTotal (x:xs) = areaTotal(x) + areaTotal(xs)
-}




--50)
data Equipamento = Bom | Razoavel | Avariado
                  deriving (Eq,Show)

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0 
naoReparar (x:xs) | x == Avariado = naoReparar xs
                  | otherwise = 1 + naoReparar xs