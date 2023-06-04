
data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show

data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa
    deriving Show
-- 1)

p1 = Capa Anchoas (Capa Cebolla (Capa Jamon (Capa Queso (Capa Salsa (Capa (Aceitunas 5) Prepizza)))))

p2 = Capa Queso (Capa Queso (Capa Queso (Capa Queso (Capa Queso (Capa Queso Prepizza)))))


cantCapasQue :: (Ingrediente -> Bool) -> Pizza -> Int
cantCapasQue _ Prepizza      = 0
cantCapasQue f (Capa i p)    = delta (f i) + cantCapasQue f p

delta :: Bool -> Int
delta False = 0 
delta True  = 1

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas _ Prepizza    = Prepizza
conCapasTransformadas f (Capa i p)  = Capa (f i) (conCapasTransformadas f p)

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue _ Prepizza   = Prepizza
soloLasCapasQue f (Capa i p) = if f i
                                then Capa i (soloLasCapasQue f p)
                                else soloLasCapasQue f p 


-- 2)

sinLactosa :: Pizza -> Pizza
sinLactosa = soloLasCapasQue (not . esLactosa)

esLactosa :: Ingrediente -> Bool 
esLactosa Queso = True
esLactosa _     = False

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa p = cantCapasQue esLactosa p /= 0  

cantidadDeQueso :: Pizza -> Int
cantidadDeQueso = cantCapasQue (sonIngredientesIguales Queso)

sonIngredientesIguales :: Ingrediente -> Ingrediente -> Bool
sonIngredientesIguales Queso Queso  = True
sonIngredientesIguales Jamon Jamon  = True
sonIngredientesIguales Salsa Salsa  = True
sonIngredientesIguales Anchoas Anchoas = True
sonIngredientesIguales Cebolla Cebolla = True
sonIngredientesIguales (Aceitunas _) (Aceitunas _) = True
sonIngredientesIguales _ _          = False

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = conCapasTransformadas doblarAceitunas

doblarAceitunas :: Ingrediente -> Ingrediente
doblarAceitunas (Aceitunas n)   = Aceitunas (n+n)
doblarAceitunas i               = i 


-- 3)
pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada _ base Prepizza  = base
pizzaProcesada f base (Capa i p)= f i (pizzaProcesada f base p)

-- 4)
cantCapasQue' :: (Ingrediente -> Bool) -> Pizza -> Int
cantCapasQue' f = pizzaProcesada (((+) . delta) . f) 0

-- cantCapasQue'' :: (Ingrediente -> Bool) -> Pizza -> Int
-- cantCapasQue'' f = pizzaProcesada ((.) ((+) . delta))

conCapasTransformadas' :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas' f = pizzaProcesada (Capa . f) Prepizza


soloLasCapasQue' :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' f = pizzaProcesada (subst capaQueCumple f) Prepizza

subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst f g x = f x (g x) 

capaQueCumple :: Ingrediente -> Bool -> Pizza -> Pizza
capaQueCumple _ False = id
capaQueCumple i True  = Capa i


-- 5)
cantidadAceitunas :: Pizza -> Int
capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
conDescripcionMejorada :: Pizza -> Pizza
conCapasDe :: Pizza -> Pizza -> Pizza
primerasNCapas :: Int -> Pizza -> Pizza

cantidadAceitunas = pizzaProcesada ((+) . aceitunas) 0

aceitunas :: Ingrediente -> Int
aceitunas (Aceitunas i) = i
aceitunas _             = 0 

capasQueCumplen f = pizzaProcesada (subst ingredienteQueCumple f) []

ingredienteQueCumple :: Ingrediente -> Bool -> [Ingrediente] -> [Ingrediente]
ingredienteQueCumple _ False = id
ingredienteQueCumple i True  = (i:)


conDescripcionMejorada = pizzaProcesada juntarAceitunas Prepizza

juntarAceitunas :: Ingrediente -> Pizza -> Pizza
juntarAceitunas (Aceitunas a1) (Capa (Aceitunas a2) p) = Capa (Aceitunas (a1 + a2)) p
juntarAceitunas ing           p                       = Capa ing p

conCapasDe = flip (pizzaProcesada Capa)

-- primerasNCapas = flip (pizzaProcesada agregarNCapas (const Prepizza)) 

primerasNCapas = flip (pizzaProcesada agregarNCapas (const Prepizza))
    where agregarNCapas x h = \n -> if n==0 then Prepizza else Capa x (h (n-1))

-- agregarNCapas recibe un ingrediente y una pizza, 
-- y devuelve una funcion que dado un numero devuelve una pizza



-- 7)

map2 :: (a -> b) -> [a] -> [b]
map2 f []    = []
map2 f (x:xs)= f x : map f xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f []     = []
filter2 f (x:xs) = singularSi x (f x) ++ filter f xs

singularSi :: a -> Bool -> [a]
singularSi _ False = []
singularSi x True  = [x] 

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f base []     = base
foldr2 f base (x:xs) = f x (foldr f base xs) 

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr base f []      = base
recr base f (x:xs)  = f x xs (recr base f xs)

-- recr z f = 