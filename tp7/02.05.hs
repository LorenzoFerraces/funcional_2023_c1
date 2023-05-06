data Fila a = Fin | Celda Int a (Fila a)

-- cuenta la cantidad de veces que aparece un elemento que cumple el predicado
countF :: (a -> Bool) -> Fila a -> Int
countF  _ Fin           = 0
countF  f (Celda n x c) = sumarSi (f x) (countF f c) n

sumarSi :: Bool -> Int -> Int -> Int
sumarSi b x y = if b 
                  then x + y
                  else x

-- le suma N a los elementos donde el predicado da verdadero
sumarN :: (a -> Bool) -> Int -> Fila a -> Fila a
sumarN  _ _  Fin            = Fin
sumarN  f n  (Celda m x c)  = Celda (sumarSi (f x) m n) x (sumarN f n c)

-- Junta dos filas manteniendo el orden de los elementos
concatenarF :: Fila a -> Fila a -> Fila a
concatenarF Fin             fl  = fl
concatenarF (Celda n x c)   fl  = Celda n x (concatenarF c fl)

-- transforma cada elemento aplicando una función a los mismos
mapF :: (a -> b) -> Fila a -> Fila b
mapF  _ Fin           = Fin
mapF  f (Celda n x c) = Celda n (f x) (mapF f c)

-- transforma una fila de filas en una fila
aplanar :: Fila (Fila a) -> Fila a 
aplanar Fin           = Fin
aplanar (Celda n x c) = concatenarF (expandirCelda n x) (aplanar c)

expandirCelda :: Int -> Fila a -> Fila a 
expandirCelda _ Fin           = Fin
expandirCelda n (Celda m x c) = Celda (m * n) x (expandirCelda n c)

-- los elementos iguales los colapsa en una misma posición (sean contiguos o no)
-- comprimir :: Fila a -> Fila a

-- denota la composición de las funciones manteniendo el orden de aparición y aplicandolas las veces que aparezca
componer :: Fila (a -> a) -> (a -> a)
componer Fin            = id
componer (Celda n x c)  = many n x . componer c

many :: Int -> (a -> a) -> (a -> a)
many 0  _ = id
many n  f = f . many (n-1) f 