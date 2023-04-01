{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Use bimap" #-}

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
apply f = f 

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

--4)
{-

--a.
apply apply apply :: (a -> b) -> a -> b


--b.
(twice doble) 2 :: Int


--c.
twice twice twice swap :: (a, b) -> (a, b)

--d.
flip twice 1 doble :: Int
-}


--5)

--a. 
{-
appDup f = g 
    where g x = f (x, x)
-}

appDup :: ((a,a) -> b) -> a -> b 
appDup = \f -> \x -> f(x, x)

--b.
{-
appFork (f, g) = h 
    where h x = (f x, g x)
-}
appFork :: (a -> b, a -> c) -> a -> (b, c)
appFork = \(f,g) -> \x -> (f x, g x)
--c.
{-
appPar (f, g) = h 
    where h (x, y) = (f x, g y)
-}
appPar :: (a -> b, c -> d) -> (a,c) -> (b, d)
appPar = \(f,g) -> \(x,y) -> (f x, g y)

--d.
{-
appDist f = g
    where g (x, y) = (f x, g y)
-}
appDist :: (a -> b) -> (a,a) -> (b,b)
appDist = \f -> \(x,y) -> (f x, f y)
--e.
{-
subst f = h
    where h g = k
        where k x = (f x) (g x)
-}
subst :: (a -> b) -> (a -> c) -> a -> (b,c)
subst = \f -> \g -> \x -> (f x, g x)


-- 6)