
data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA
    deriving Show

foldExpA :: (a -> a -> a) -> (a -> a -> a) -> (Int -> a) -> ExpA -> a
foldExpA f g h (Cte n)        = h n
foldExpA f g h (Suma ex1 ex2) = f (foldExpA f g h ex1) (foldExpA f g h ex2) 
foldExpA f g h (Prod ex1 ex2) = g (foldExpA f g h ex1) (foldExpA f g h ex2)

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f base []     = base
foldr2 f base (x:xs) = f x (foldr2 f base xs) 

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f base []      = base
recr f base (x:xs)  = f x xs (recr f base xs)

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

recT :: (a -> Tree a -> Tree a -> b -> b -> b) -> b -> Tree a -> b
recT _ z EmptyT = z
recT f z (NodeT x ti td) = f x ti td (recT f z ti) (recT f z td)

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

todosLosCaminos = foldT agregarRaiz []
    where agregarRaiz x tis tds = (foldr2 ((:) . (x:)) [[x]]) (tis ++ tds)

todosLosNiveles = foldT juntarNivel []
    where juntarNivel x tis tds = [x] : juntarListas2 tis tds

nivelN = (foldT tomarNivel (const []))
    where tomarNivel x tis tds n = case n==0 of 
                                        True -> [x] 
                                        _    -> (tis (n-1)) ++ (tds (n-1))


data GTree a = GNode a [GTree a]
    deriving Show

gtPrueba :: GTree Int
gtPrueba = GNode 10 [ GNode 2 [ GNode 5 [], GNode 6 [] ] , GNode 3 [ GNode 7 [] ] , GNode 4 [] ]


-- foldGT0 :: (a->[b]->b) -> GTree a -> b
-- foldGT0 h (GNode x ts) = h x (map (foldGT0 h) ts)

-- recGT0 :: (a -> [GTree a] -> [b] -> b) -> GTree a -> b

-- recGT1 ::

-- sumGT0 :: GTree Int -> Int
-- sumGT0 = foldGT0 (flip ((+) . sum))


-- foldGT1 :: (a->c->b) -> (b->c->c) -> c -> GTree a -> b
-- foldGT1 g f z (GNode x ts) = g x (foldr f z (map (foldGT1 g f z) ts))

-- sumGT1 :: GTree Int -> Int
-- sumGT1 = foldGT1 (+) (+) 0


foldGT :: (a -> c -> b) -> ([b] -> c) -> GTree a -> b
foldGT g k (GNode x ts) = g x (k(map (foldGT g k) ts))

mapGT :: (a -> b) -> GTree a -> GTree b
mapGT f = foldGT (GNode . f) id

sumGT :: GTree Int -> Int
sumGT = foldGT (+) sum

sizeGT :: GTree a -> Int
sizeGT = foldGT ((+) . const 1) sum


heightGT :: GTree a -> Int
heightGT = foldGT ((+) . const 1) (maxOr 0) 
    where maxOr x [] = 0
          maxOr x xs = maximum xs

preOrderGT :: GTree a -> [a]
preOrderGT = foldGT (:) concat

postOrderGT :: GTree a -> [a]
postOrderGT = foldGT  ((flip (++)) . (:[])) (concat . reverse) --Definido como foldGT

postOrderGT' :: GTree a -> [a]
postOrderGT' = reverse . preOrderGT --Mejor version

mirrorGT :: GTree a -> GTree a
mirrorGT = foldGT GNode reverse

countByGT :: (a -> Bool) -> GTree a -> Int
countByGT p = foldGT ((+) . (delta . p)) sum

partitionGT :: (a -> Bool) -> GTree a -> ([a], [a])
partitionGT p = foldGT agregar concatPar
    where agregar x ps = case p x of 
                            True -> mixPares ([x], []) ps
                            False -> mixPares ([], [x]) ps

mixPares :: ([a], [b]) -> ([a], [b]) -> ([a],[b])
mixPares (x,y) p = overFst (x++) (overSnd (y++) p)

concatPar :: [([a], [b])] -> ([a], [b])
concatPar = foldr2 mixPares ([],[])

-- zipWithGT :: (a->b->c) -> GTree a -> GTree b -> GTree c
-- zipWithGT f = foldGT combinar 
--     where combinar x rts (GNode y ts) = GNode (f x y) (rts ts)

caminoMasLargoGT :: GTree a -> [a]
caminoMasLargoGT = foldGT (:) maximumByLength

maximumByLength :: [[a]] -> [a]
maximumByLength = recr elMasLargo []
                    where elMasLargo x [] rs = x
                          elMasLargo x (ys:yss) r =  case length x > length r of
                                                        True -> x
                                                        _    -> r


todosLosCaminosGT :: GTree a -> [[a]]
todosLosCaminosGT = foldGT agregarRaiz concat
                where agregarRaiz x ys = foldr2 ((:) . (x:)) [[x]] ys

-- todosLosNivelesGT :: GTree a -> [[a]]
-- todosLosNivelesGT = foldGT agregarN concat
--                 where agregarN x ys =

-- caminoHastaGT :: Eq a => a -> GTree a -> [a]
-- nivelNGT :: GTree a -> Int -> [a]

xsss1 :: [[[Int]]]
xsss1 = [[[1,2,3,4], [5,6,7,8], [9,10,11]]]
