--Pruebas

doble :: Int -> Int
doble x = x + x

--Ejercicio 1 

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y

-- Ejercicio 2

-- a. 
apply :: (a -> b) -> a -> b
apply f x = f x

-- b.
twice :: (a -> a) -> a -> a
twice f x = f(f x)

-- c.
id :: a -> a
id x = x 

-- d.
flip :: (a -> (b -> c)) -> (b -> (a -> c))
flip f x y = f y x 

-- e.
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x) 

uflip :: ((a,a) -> c) -> (a,a) -> c
uflip f p = f (swap p)

-- f.
const :: a -> b -> a
const x y = x 

-- g.
compose :: (a -> b) -> (c -> a) -> c -> b
compose f g x = f (g x)