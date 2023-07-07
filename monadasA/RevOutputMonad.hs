module RevOutputMonad where
import Monadas

type Screen = String
data RevOutput a = O2 (Screen, a) deriving Show

instance Monad RevOutput where
  return x = O2 ("", x)
  m >>= k = let O2 (scr1, v) = m
              in let O2 (scr2, res) = k v
                  in O2 (scr2 ++ scr1, res)
  fail msg = error msg

instance PrintMonad RevOutput where
  printf msg = O2 (msg ++ "\n", ())