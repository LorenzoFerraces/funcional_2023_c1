module OutputMonad where
import Monadas

type Screen = String
data Output a = O (a, Screen) deriving Show

instance Monad Output where
  return x = O (x, "")
  m >>= k = let O (v, scr1) = m
              in let O (res, scr2) = k v
                in O (res, scr1 ++ scr2)
  fail msg = error msg

instance PrintMonad Output where
  printf msg = O ((), msg ++ "\n")