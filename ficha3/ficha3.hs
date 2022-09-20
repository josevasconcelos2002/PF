module Ficha3 where


-- 1)

data Hora = H Int Int
        deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- a)

testa_h :: Hora -> Bool
testa_h (H a b)    | a >= 00 && a <= 23 && b >= 00 && b <= 59 = True
                   | otherwise = False

maior :: Etapa -> Bool
maior (H a b , H c d) | a > c = False
                      | a == c && b >= d = False
                      | otherwise = True

testa_etapa :: Etapa -> Bool
testa_etapa (H a b , H c d) | testa_h (H a b) && testa_h (H c d) && maior(H a b , H c d) = True
                            | otherwise = False

-- b)
{-
viagem_depois :: Viagem -> Bool
viagem_depois [] = False
viagem_depois (((H a b , H c d),(H e f , H g i)):h) | c > e = False
                                                    | c == e && d > f = False
                                                    | otherwise = True


testa_viagem :: Viagem -> Bool
testa_viagem [] = False
testa_viagem ( H a b , H c d) | testa_etapa(H a b , H c d) = True
                              | otherwise = False
testa_viagem ((H a b , H c d),(H e f , H g i):h) | testa_etapa(H a b , H c d) && testa_etapa(H e f , H g i) && viagem_depois ((H a b , H c d),(H e f , H g i)) = True && testa_viagem h
                                                 | otherwise = False
-}

-- 3)
{-
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]
-}
-- a) 
{-
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail [] _ a = a 
acrescEmail n e ((y,(x:xs)):h) = (((y,(x:xs)):h):(n,[Email e]))
-}

-- 4)
type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String

data Data = D Dia Mes Ano
          deriving Show

type TabDN = [(Nome,Data)]

-- a)

procura :: Nome -> TabDN -> Maybe Data
procura [] _ = Nothing
procura n [(x,D d m a)] | n == x = Just (D d m a)
                        | otherwise = Nothing
procura n ((x,D d m a):h) | n == x = Just (D d m a)
                          | otherwise = procura n h

-- b)

idade :: Data -> Nome -> TabDN -> Maybe Int
idade (D d m a) n ((x,D w y z):h) | n == x && a == z = Just 0
                                  | n == x && a>z && m>=y && d>=w = Just (a-z)
                                  | n == x && a>z && m<y = Just (a-z-1)
                                  | n == x && a>z && m==y && d<w = Just (a-z-1) 
                                  | n == x && a<z = Nothing
                                  | otherwise = idade (D d m a) n h

-- c)

anterior :: Data -> Data -> Bool
anterior (D d m a) (D x y z) | z>a = True
                             | z == a && y>m = True
                             | z == a && y == m && x>d = True
                             | otherwise = False

-- d)

ordena :: TabDN -> TabDN
ordena [] = []
ordena [(n,D d m a),(q,D x y z)] | (anterior (D d m a) (D x y z)) == True = [(n,D d m a),(q,D x y z)]
                                 | otherwise = [(q,D x y z),(n,D d m a)]
{-
ordena ((n,D d m a),(q,D x y z):h) | (anterior (D d m a) (D x y z)) == True = ((n,D d m a),(q,D x y z)) : ordena h  //como fazer para mais de dois TabDN?
                                   | otherwise = ((q,D x y z),(n,D d m a)): ordena h
-}

-- e)
{-
porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade (D _ _ _) [] = []
porIdade (D d m a) (((x,D w y z),(t,D k l p)):h) | idade1(D d m a) [(x,D w y z)] >= idade1(D d m a) [(t,D k l p)] = ((t,D k l p),(x,D w y z)):porIdade (D d m a) h  
                                                 | otherwise = ((x,D w y z),(t,D k l p)):porIdade (D d m a) h
idade1 :: Data -> TabDN -> Int
idade1 (D _ _ _) [] = 0
idade1 (D d m a) [(n,D w y z)] | a == z = 0
                               | a>z && m>=y && d>=w = a-z
                               | a>z && m<y = a-z-1
                               | a>z && m==y && d<w = a-z-1
-}
-- 5)
{-
data Movimento = Credito Float | Debito Float
                deriving Show

data Data = D Int Int Int
            deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
                 deriving Show
-}
-- a)