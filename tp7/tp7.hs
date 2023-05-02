{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}

data Pizza = Prepizza | Capa Ingrediente Pizza

data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa


--a
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza        = 0
cantidadDeCapas (Capa i p)  = cantidadDeIngrediente i + cantidadDeCapas p

cantidadDeIngrediente :: Ingrediente -> Int
cantidadDeIngrediente (Aceitunas i) = i
cantidadDeIngrediente i = 1


--b
cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza    = 0
cantidadDeAceitunas (Capa i p)  = cantAceitunasEnCapa i + cantidadDeAceitunas p


cantAceitunasEnCapa :: Ingrediente -> Int
cantAceitunasEnCapa (Aceitunas i) = i
cantAceitunasEnCapa _ = 0

--c
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas (Capa i p)    = Capa (duplicarAceitunasEnCapa i) p
duplicarAceitunas p             = p

duplicarAceitunasEnCapa :: Ingrediente -> Ingrediente
duplicarAceitunasEnCapa (Aceitunas i)   = Aceitunas (i*2)
duplicarAceitunasEnCapa i               = i

--d
sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza = Prepizza
sinLactosa (Capa i c) = case i of 
                        Queso   -> sinLactosa c
                        p       -> Capa i (sinLactosa c)

--e
aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza    = True
aptaIntolerantesLactosa (Capa i c)  = not (esQueso i) && aptaIntolerantesLactosa c

esQueso :: Ingrediente -> Bool
esQueso Queso   = True
esQueso p       = False

--f

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza = Prepizza
conDescripcionMejorada (Capa i p)   = juntarAceitunas i (conDescripcionMejorada p)


juntarAceitunas :: Ingrediente -> Pizza -> Pizza
juntarAceitunas i Prepizza    = Capa i Prepizza
juntarAceitunas i1 (Capa i2 p)  = if esAceituna i2 && esAceituna i1
                                    then Capa (sumarAceitunas i1 i2) p
                                    else Capa i1 (Capa i2 p)

ingredienteDe :: Pizza -> Ingrediente
ingredienteDe (Capa i p) = i

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas i)    = True
esAceituna _                = False 



sumarAceitunas :: Ingrediente -> Ingrediente -> Ingrediente
sumarAceitunas a1 a2 = Aceitunas (cantAceitunasEnCapa a1 + cantAceitunasEnCapa a2)






--Seccion 2

type Nombre = String
data Planilla = Fin | Registro Nombre Planilla
data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo

{-
reglas Planilla

regla 1 = Fin existe en el conjunto
regla 2 = si p estan en el conjunto, (Registro Nombre p) esta en el conjunto
-}

{-
reglas Equipo

regla 1 = Becario n existe en el conjunto
regla 2 = si e1, e2, e3 estan en el conjunto,  (Investigador Nombre e1 e2 e3) esta en el conjunto
-}



{-
Estructura recursion

f :: Planilla -> a
f Fin               = ...
f (Registro n p)    = ... n ... f p


eqp :: Equipo -> a
eqp (Becario n) = ...
eqp (Investigador n e1 e2 e3) = ... n ... eqp e1 ... eqp e2 ... eqp e3 
-}


--a
largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin             = 0
largoDePlanilla (Registro n p) = 1 + largoDePlanilla p

--b
esta :: Nombre -> Planilla -> Bool
esta n Fin              = False
esta n (Registro n2 p)   = n == n2 || esta n p 

--c
juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin            p2   = p2
juntarPlanillas p1             Fin  = p1
juntarPlanillas (Registro n p) p2   = Registro n (juntarPlanillas p p2)

--d
nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario _)                  = 1
nivelesJerarquicos (Investigador _ e1 e2 e3)    = 1 + max (nivelesJerarquicos e3) (max (nivelesJerarquicos e1) (nivelesJerarquicos e2)) 

--e
cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario _)               = 1
cantidadDeIntegrantes (Investigador _ e1 e2 e3) = 1 + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2 + cantidadDeIntegrantes e3 

--f
planillaDeintegrantes :: Equipo -> Planilla
planillaDeintegrantes (Becario n)               = Registro n Fin
planillaDeintegrantes (Investigador n e1 e2 e3) = Registro n (juntarPlanillas (planillaDeintegrantes e1) (juntarPlanillas (planillaDeintegrantes e2) (planillaDeintegrantes e2)))






--Seccion 3

data Dungeon a = Habitacion a | Pasaje (Maybe a) (Dungeon a) | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a)

{-

Reglas Dungeon

1- Habitacion a existe en el conjunto
2- Si d existe en el conjunto, entonces Pasaje (Maybe a) d existe en el conjunto
3- Si d1, d2 existen en el conjunto, entonces Bifurcacion (Maybe a) d1 d2 existe en el conjunto

-}

{-
Estructura recursion:

d :: Dungeon a -> b
d (Habitacion a)            = ... a
d (Pasaje m du)             = ... m ... d du
d (Bifurcacion m du1 du2)   = ... m ... d du1 ... d du2
-}

--a
cantidadDeBifurcaciones :: Dungeon a -> Int
cantidadDeBifurcaciones (Habitacion a)          = 0
cantidadDeBifurcaciones (Pasaje _ d)            = cantidadDeBifurcaciones d
cantidadDeBifurcaciones (Bifurcacion _ d1 d2)   = 1 + cantidadDeBifurcaciones d1 + cantidadDeBifurcaciones d2 

--b
cantidadDePuntosInteresantes :: Dungeon a -> Int
cantidadDePuntosInteresantes (Habitacion a)         = 1
cantidadDePuntosInteresantes (Pasaje _ d)           = 1 + cantidadDePuntosInteresantes d
cantidadDePuntosInteresantes (Bifurcacion _ d1 d2)  = 1 + cantidadDePuntosInteresantes d1 + cantidadDePuntosInteresantes d2

--c
cantidadDePuntosVacios :: Dungeon a -> Int
cantidadDePuntosVacios (Habitacion a)           = 0
cantidadDePuntosVacios (Pasaje m d)             = unoSi (esNothing m) + cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion m d1 d2)    = unoSi (esNothing m) + cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

esNothing :: Maybe a -> Bool
esNothing Nothing   = True
esNothing _         = False

--d
cantidadDePuntoscon :: Eq a => Dungeon a -> a -> Int
cantidadDePuntoscon (Habitacion a) obj      = unoSi (a==obj)
cantidadDePuntoscon (Pasaje m d) obj        = unoSi (esObjDeMaybe m obj) + cantidadDePuntoscon d obj
cantidadDePuntoscon (Bifurcacion m d1 d2) obj = unoSi (esObjDeMaybe m obj) + cantidadDePuntoscon d1 obj + cantidadDePuntoscon d2 obj

esObjDeMaybe :: Eq a => Maybe a -> a -> Bool 
esObjDeMaybe Nothing _      = False 
esObjDeMaybe (Just a) obj   = a == obj

--e
esLineal :: Dungeon a -> Bool
esLineal (Habitacion a)         = True
esLineal (Bifurcacion _ _ _)    = False 
esLineal (Pasaje _ d)           = esLineal d

--f
llenoDe :: Eq a => Dungeon a -> a -> Bool
llenoDe (Habitacion a) obj          = a == obj
llenoDe (Pasaje m d) obj            = esObjDeMaybe m obj && llenoDe d obj
llenoDe (Bifurcacion m d1 d2) obj   = esObjDeMaybe m obj && llenoDe d2 obj && llenoDe d2 obj 

