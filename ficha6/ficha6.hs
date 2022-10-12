data BTree a = Empty
             | Node a (BTree a) (BTree a)
        deriving Show

--exercicio 1
--a)
altura :: BTree a -> Int
altura Empty = 0
altura (Node _ e d) = max (1+altura e) (1+altura d)