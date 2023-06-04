{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
f1 x = let (y,z) = (x,x) in y

f2 (x,y) = let z = x + y in g (z,y) where g (a,b) = a-b

f3 p = case p of (x,y) -> x

f = \p -> let (x,y) = p in y