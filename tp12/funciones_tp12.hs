import Language.Haskell.TH (Exp)
data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA

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

showExpA' :: ExpA -> String
showExpA' foldExpA 


