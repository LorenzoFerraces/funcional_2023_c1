{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
data EA = Const Int | BOp BinOp EA EA
data BinOp = Sum | Mul

--clase 8 
data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA

evalEA :: EA -> Int
evalEA (Const i)        =   i
evalEA (BOp op ea1 ea2) =   evalBinOp op (evalEA ea1) (evalEA ea2) 

evalBinOp :: BinOp -> Int -> Int -> Int
evalBinOp Sum  =   (+)
evalBinOp Mul  =   (*)


ea2ExpA :: EA -> ExpA
ea2ExpA (Const i)        = Cte i
ea2ExpA (BOp op ea1 ea2) = binOp2ExpA op (ea2ExpA ea1) (ea2ExpA ea2)



binOp2ExpA:: BinOp -> ExpA -> ExpA -> ExpA
binOp2ExpA Sum = Suma 
binOp2ExpA Mul = Prod 



expA2ea :: ExpA -> EA
expA2ea (Cte i)         =   Const i
expA2ea (Suma exp1 exp2)=   BOp Sum (expA2ea exp1) (expA2ea exp2)
expA2ea (Prod exp1 exp2)=   BOp Mul (expA2ea exp1) (expA2ea exp2)


-- ej 2

data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b)

cantidadDeHojas :: Arbol a b -> Int
cantidadDeHojas (Hoja _)         = 1
cantidadDeHojas (Nodo _ ab1 ab2) = cantidadDeHojas ab1 + cantidadDeHojas ab2


cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja _)          = 0
cantidadDeNodos (Nodo _ ab1 ab2)  = 1 + cantidadDeNodos ab1 + cantidadDeNodos ab2
 
cantidadDeConstructores :: Arbol a b -> Int
cantidadDeConstructores (Hoja _)          = 1
cantidadDeConstructores (Nodo _ ab1 ab2)  = 1 + cantidadDeConstructores ab1 + cantidadDeConstructores ab2
 
ea2Arbol :: EA -> Arbol BinOp Int
ea2Arbol (Const i)        = Hoja i
ea2Arbol (BOp op ea1 ea2) = Nodo op (ea2Arbol ea1) (ea2Arbol ea2)

--ej 3
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

sumarT :: Tree Int -> Int
sumarT EmptyT           = 0
sumarT (NodeT i t1 t2)  = i + sumarT t1 + sumarT t2

sizeT :: Tree a -> Int
sizeT EmptyT           = 0
sizeT (NodeT _ t1 t2)  = 1 + sizeT t1 + sizeT t2

anyT :: (a -> Bool) -> Tree a -> Bool
anyT _ EmptyT           = False
anyT f (NodeT i t1 t2)  = f i || anyT f t1 || anyT f t2 

countT :: (a -> Bool) -> Tree a -> Int
countT _ EmptyT           = 0
countT f (NodeT i t1 t2)  = unoSi (f i) + countT f t1 + countT f t2 

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0


countLeaves :: Tree a -> Int
countLeaves EmptyT          = 0
countLeaves (NodeT _ t1 t2) = unoSi (esEmptyT t1 && esEmptyT t2) + countLeaves t1 + countLeaves t2

esEmptyT :: Tree a -> Bool
esEmptyT EmptyT  = True
esEmptyT _       = False

heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT _ t1 t2) = 1 + max (heightT t1) (heightT t2)

inOrder :: Tree a -> [a]
inOrder EmptyT          = []
inOrder (NodeT i t1 t2) = inOrder t1 ++ [i] ++ inOrder t2

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT           = []
listPerLevel (NodeT i t1 t2)  = [i] : zipWith (++) (listPerLevel t1) (listPerLevel t2)


mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT i t1 t2) = NodeT i (mirrorT t2) (mirrorT t1)

levelN :: Int -> Tree a -> [a]
levelN 0 (NodeT i _ _)  = [i]
levelN n (NodeT _ t1 t2)  = levelN (n-1) t1 ++ levelN (n-1) t2
levelN _ EmptyT           = []

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT           = []
ramaMasLarga (NodeT i t1 t2)  = i : longest (ramaMasLarga t1) (ramaMasLarga t2)

longest :: [a] -> [a] -> [a]
longest xs ys = if length xs > length ys 
                  then xs
                  else ys

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT i t1 t2) = agregarAListas i (todosLosCaminos t1 ++ todosLosCaminos t2)

agregarAListas :: a -> [[a]] -> [[a]]
agregarAListas x []       = [[x]]
agregarAListas x (xs:xss) = (x:xs) : agregarAListas x xss 
