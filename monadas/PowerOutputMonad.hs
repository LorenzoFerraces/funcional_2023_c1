module PowerOutputMonad where
import Monadas

type PowerScreen = [ String ]
data PowerOutput a = O3 (a, PowerScreen) deriving Show

instance Monad PowerOutput where
  return x = O3 (x, [])
  m >>= k = let O3 (v, scr1) = m
              in let O3 (res, scr2) = k v
                  in O3 (res, scr1 ++ scr2)
  fail msg = error msg

instance PrintMonad PowerOutput where
  printf msg = O3 ((), [ msg ])