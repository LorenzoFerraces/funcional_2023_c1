{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
data NExp = Var Variable | NCte Int | NBOp NBinOp NExp NExp
data NBinOp = Add | Sub | Mul | Div | Mod | Pow
type Variable = String

data Mem -- Tipo abstracto de datos
-- memf :: Mem
-- memg :: String -> Mem -> Maybe TA
-- memh :: String -> TA -> Mem -> Mem
-- memk :: Mem -> [ String ]

cfNExp :: NExp -> NExp
cfNExp (Var x) = Var x
cfNExp (NCte n) = NCte n
cfNExp (NBOp bop ne1 ne2) = cfNBOp bop (cfNExp ne1) (cfNExp ne2)

evalNExp :: NExp -> (Mem -> Int)
evalNExp (Var x) = \mem -> cuantoVale x mem
evalNExp (NCte n) = \mem -> n
evalNExp (NBOp bop ne1 ne2) = \mem -> evalNBOp bop (evalNExp ne1 mem) (evalNExp ne2 mem)

evalNBOp :: NBinOp -> (Int -> Int -> Int)
evalNBOp Add = (+)
evalNBOp Sub = (-)
evalNBOp Mul = (*)
evalNBOp Div = div
evalNBOp Mod = mod
evalNBOp Pow = (^)

cfNBOp :: NBinOp -> NExp -> NExp -> NExp
cfNBOp bop (NCte n) (NCte m)  = NCte (evalNBOp bop n m)
cfNBOp bop nc1      nc2       = NBOp bop nc1 nc2            