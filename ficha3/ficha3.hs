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





-- 5)

data Movimento = Credito Float | Debito Float
                deriving Show

data Data = D Int Int Int
            deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
                 deriving Show

-- a)

