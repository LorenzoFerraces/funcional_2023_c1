data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA
    deriving Show

exp = Suma

foldExpA :: (a -> a -> a) -> (a -> a -> a) -> (Int -> a) -> ExpA -> a
foldExpA f g h (Cte n)        = h n
foldExpA f g h (Suma ex1 ex2) = f (foldExpA f g h ex1) (foldExpA f g h ex2) 
foldExpA f g h (Prod ex1 ex2) = g (foldExpA f g h ex1) (foldExpA f g h ex2)

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f base []     = base
foldr2 f base (x:xs) = f x (foldr2 f base xs) 

many :: Int -> (a -> a) -> a -> a 
many 0 f x = x
many n f x = f (many (n-1) f x) 

delta :: Bool -> Int
delta False = 0 
delta True  = 1

overFst :: (a -> a) -> (a, b) -> (a , b)
overFst f (x, y) = (f x , y)

overSnd :: (b -> b) -> (a, b) -> (a , b)
overSnd f (x, y) = (x , f y)

appPars :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
appPars f (x,y) (w,z) = (f x w, f y z)

isEmptyT :: Tree a -> Bool
isEmptyT (NodeT _ _ _ ) = False
isEmptyT _              = True

leftTree :: Tree a -> Tree a 
leftTree (NodeT x ti td) = ti

rightTree :: Tree a -> Tree a 
rightTree (NodeT x ti td) = td

root :: Tree a -> a 
root (NodeT x ti td) = x

juntarListas2 :: [[a]] -> [[a]] -> [[a]]
juntarListas2 = foldr2 concatenar id
    where concatenar xs rss yss = case null yss of
                                    True -> xs : rss yss
                                    _    -> (xs ++ (head yss)) : rss (tail yss) 

zipWith4 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith4 f = foldr2 combinar (const [])
    where combinar x g ys = if null ys 
                                then g []
                                else f x (head ys) : g (tail ys)


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
pruebaT :: Tree Int
pruebaT = NodeT 8 
            (NodeT 5
                (NodeT 3
                    (NodeT 1 
                        EmptyT
                        (NodeT 2
                            EmptyT 
                            EmptyT))
                    (NodeT 4 
                        EmptyT
                        EmptyT))
                (NodeT 7
                    (NodeT 6
                        EmptyT
                        EmptyT)
                    EmptyT))
            (NodeT 10
                (NodeT 9
                    EmptyT
                    EmptyT)
                EmptyT)

foldT :: (a -> b -> b -> b) -> b -> Tree a -> b 
foldT _ z EmptyT = z
foldT f z (NodeT x ti td) = f x (foldT f z ti) (foldT f z td)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT (NodeT . f) EmptyT

sumT :: Tree Int -> Int
sizeT :: Tree a -> Int
heightT :: Tree a -> Int
preOrder :: Tree a -> [a]
inOrder :: Tree a -> [a]
postOrder :: Tree a -> [a]
mirrorT :: Tree a -> Tree a
countByT :: (a -> Bool) -> Tree a -> Int
partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
zipWithT :: (a->b->c) -> Tree a -> Tree b -> Tree c
caminoMasLargo :: Tree a -> [a]
todosLosCaminos :: Tree a -> [[a]]
todosLosNiveles :: Tree a -> [[a]]
nivelN :: Tree a -> Int -> [a]

sumT = foldT g 0
    where g n m l = n + m + l

sizeT = foldT g 0
    where g x n m  = succ (n +m) 

heightT = foldT g 0 
    where g x n m = succ (max n m)

preOrder = foldT g []
    where g a xs ys = a : (xs ++ ys)

inOrder = foldT g []
    where g a xs ys = xs ++ [a] ++ ys

postOrder = foldT g []
    where g a xs ys = (ys ++ xs) ++ [a]

mirrorT = foldT g EmptyT
    where g x ti td = NodeT x td ti

countByT f = foldT g 0
    where g x n m = delta (f x) + (n + m)

partitionT f = foldT partir ([], [])
    where partir x ps1 ps2 = case f x of
                            True -> overFst (x:) (appPars (++) ps1 ps2)
                            False -> overSnd (x:) (appPars (++) ps1 ps2)

zipWithT f = foldT combinar (const EmptyT)
    where combinar x ti td t2 = case isEmptyT t2 of 
                                    True -> EmptyT
                                    _    -> NodeT (f x (root  t2)) (ti (leftTree t2)) (td (rightTree t2))

caminoMasLargo = foldT masLargo []
    where masLargo x tis tds = case length tis > length tds of
                                    True -> x : tis
                                    _    -> x : tds

todosLosCaminos = foldT agregar []
    where agregar x tis tds = (foldr2 ((:) . (x:)) [[x]]) (tis ++ tds)

todosLosNiveles = foldT juntar []
    where juntar x tis tds = [x] : juntarListas2 tis tds

nivelN = (foldT tomarNivel (const []))
    where tomarNivel x tis tds n = case n==0 of 
                                        True -> [x] 
                                        _    -> (tis (n-1)) ++ (tds (n-1))