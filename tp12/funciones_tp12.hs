
data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA
    deriving Show

exp = Suma

foldExpA :: (a -> a -> a) -> (a -> a -> a) -> (Int -> a) -> ExpA -> a
foldExpA f g h (Cte n)        = h n
foldExpA f g h (Suma ex1 ex2) = f (foldExpA f g h ex1) (foldExpA f g h ex2) 
foldExpA f g h (Prod ex1 ex2) = g (foldExpA f g h ex1) (foldExpA f g h ex2)

delta :: Bool -> Int
delta False = 0 
delta True  = 1

cantidadDeCeros :: ExpA -> Int
cantidadDeCeros = foldExpA (+) (+) (delta . (==0))

noTieneNegativosExplicitosExpA :: ExpA -> Bool
noTieneNegativosExplicitosExpA = foldExpA (&&) (&&) (>=0)

simplificarExpA' :: ExpA -> ExpA 
simplificarExpA' = foldExpA simplificarSuma' simplificarProd' Cte

simplificarSuma' :: ExpA -> ExpA -> ExpA
simplificarSuma' (Cte 0) ex = ex
simplificarSuma' ex (Cte 0) = ex
simplificarSuma' ex1 ex2    = Suma ex1 ex2 

simplificarProd' :: ExpA -> ExpA -> ExpA
simplificarProd' (Cte 0) ex = Cte 0
simplificarProd' ex (Cte 0) = Cte 0
simplificarProd' (Cte 1) ex = ex
simplificarProd' ex (Cte 1) = ex
simplificarProd' ex1 ex2    = Prod ex1 ex2 

evalExpA' :: ExpA -> Int
evalExpA' = foldExpA (+) (*) id

-- showExpA' :: ExpA -> String
-- showExpA' foldExpA 

-- cantDeSumaCeros :: ExpA -> Int
-- cantDeSumaCeros = foldExpA (+) (const (const 0)) (const 1)

-- 3)
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
pruebaT = NodeT 5 
    (NodeT 2 
        (NodeT 1 
            (NodeT 0 EmptyT EmptyT) EmptyT) 
        (NodeT 3 EmptyT 
            (NodeT 4 EmptyT EmptyT))) 
    (NodeT 8 
        (NodeT 7 
            (NodeT 6 EmptyT EmptyT) EmptyT) 
        (NodeT 9 EmptyT EmptyT))

foldT :: (a -> b -> b -> b) -> b -> Tree a -> b 
foldT _ z EmptyT = z
foldT f z (NodeT x ti td) = f x (foldT f z ti) (foldT f z td)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT (NodeT . f) EmptyT

