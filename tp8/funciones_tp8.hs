{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Prelude hiding (unzip, zip, reverse, any, all, elem, concat, (++), product, sum, length)
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use list literal pattern" #-}
{-# HLINT ignore "Use bimap" #-}

length :: [a] -> Int
length  []      = 0
length  (x:xs)  = 1 + length xs  

sum :: [Int] -> Int
sum []      =   0
sum (x:xs)  = x + sum xs

product :: [Int] -> Int
product []      = 1
product (x:xs)  = x * product xs

concat :: [[a]] -> [a]
concat    []      = []
concat  (xs:xss)  = xs ++ concat xss

elem :: Eq a => a -> [a] -> Bool
elem a []     = False
elem a (x:xs) = a == x || elem a xs

all :: (a -> Bool) -> [a] -> Bool
all f []    = True
all f (x:xs)= f x && all f xs

any :: (a -> Bool) -> [a] -> Bool
any f []    = False
any f (x:xs)= f x || any f xs

count :: (a -> Bool) -> [a] -> Int
count f []      = 0
count f (x:xs)  = if f x
                    then 1 + count f xs
                    else count f xs

subset :: Eq a => [a] -> [a] -> Bool
subset []     ys  = True
subset (x:xs) ys  = elem x ys && subset xs ys

(++) :: [a] -> [a] -> [a]
(++) []     ys  = ys
(++) (x:xs) ys  = x : (++) xs ys

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = (++) (reverse xs) [x]

zip :: [a] -> [b] -> [(a,b)]
zip xs      []    = []
zip []      ys    = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

unzip :: [(a,b)] -> ([a], [b])
unzip []  = ([],[])
unzip ((f,sn) : ps)  =  let pls = unzip ps in
                        (f:fst pls, sn: snd pls)


--Seccion 2

data N = Z | S N

evalN :: N -> Int
evalN Z    = 0
evalN (S n)= 1 + evalN n

addN :: N -> N -> N
addN    Z   m = m
addN  (S n) m = addN n (S m)

prodN :: N -> N -> N
prodN Z     m = Z
prodN (S n) m = if esZ n
                then m
                else addN m (prodN n m)

esZ :: N -> Bool
esZ   Z   = True
esZ (S _) = False

int2N :: Int -> N
int2N 0 = Z
int2N n = S (int2N (n-1))

--Ejercicio 2

type NU = [ () ]

data Unit = Unit

evalNU :: NU -> Int
evalNU      []      = 0
evalNU ( () : us) = 1 + evalNU us 

succNU :: NU -> NU
succNU nu = () : nu

addNU :: NU -> NU -> NU
addNU     []    nu1 = nu1
addNU (() : us) nu1 = addNU us (() : nu1)

nu2n :: NU -> N
nu2n     []     = Z
nu2n  (() : us) = S (nu2n us)

n2nu :: N -> NU
n2nu    Z   = []
n2nu  (S n) = () : n2nu n

--Ejercicio 3

data DigBin = O | I

type NBin = [DigBin]

dbAsInt :: DigBin -> Int
dbAsInt I = 1 
dbAsInt O = 0 

dbOfBool :: Bool -> DigBin
dbOfBool True   = I
dbOfBool False  = O

dbAsBool :: DigBin -> Bool
dbAsBool I  = True
dbAsBool O  = False

addIf :: (a -> Bool) -> a -> [a] -> [a]
addIf f a l = if f a 
                then a:l
                else l

evalNB :: NBin -> Int
evalNB = traducirNB 0 

traducirNB :: Int -> NBin -> Int
traducirNB m (nb:[])  = dbAsInt nb * (2 ^ m)
traducirNB m (nb:nbs) = dbAsInt nb * (2 ^ m) + traducirNB (m+1) nbs

normalizarNB :: NBin -> NBin
normalizarNB  []  = []
normalizarNB  (nb:nbs)  = if hayParteSignificativa nbs
                                then nb : normalizarNB nbs
                                else addIf dbAsBool nb []

hayParteSignificativa :: NBin -> Bool
hayParteSignificativa []     = False
hayParteSignificativa (nb:nbs) = dbAsBool nb || hayParteSignificativa nbs


succNB :: NBin -> NBin
succNB nb = addNB nb [I]
 
addNB :: NBin -> NBin -> NBin
addNB nb1 nb2 = addNBConCarry nb1 nb2 O

addNBConCarry :: NBin -> NBin -> DigBin -> NBin
addNBConCarry []        (nb:nbs)  db  = if dbAsBool db
                                          then let (rslt, carry) = addDBConCarry db nb O in
                                            rslt : addNBConCarry [] nbs carry 
                                          else nb:nbs
addNBConCarry (nb:nbs)       []   db  = if dbAsBool db
                                          then let (rslt, carry) = addDBConCarry db nb O in
                                            rslt : addNBConCarry [] nbs carry 
                                          else nb:nbs
addNBConCarry (nb:nbs) (mb:mbs)   db  = let (rslt, carry) = addDBConCarry nb mb db in 
                                                              rslt : addNBConCarry nbs mbs carry

addDBConCarry :: DigBin -> DigBin -> DigBin -> (DigBin, DigBin)
addDBConCarry db1 db2 db3 = let (n1,(n2, n3)) = (dbAsInt db1, (dbAsInt db2, dbAsInt db3))in
                                                case n1 + n2 + n3 of
                                                  0 -> (O, O)
                                                  1 -> (I, O)
                                                  2 -> (O, I)
                                                  3 -> (I, I)


                                                

nb2n :: NBin -> N
nb2n = int2N . evalNB 

n2nb :: N -> NBin
n2nb = int2NB . evalN 

int2NB :: Int -> NBin
int2NB 0 = [O]
int2NB n = dbOfBool (odd n) : int2NB (div n 2)
