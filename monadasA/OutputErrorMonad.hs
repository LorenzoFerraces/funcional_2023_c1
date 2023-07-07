module OutputErrorMonad where
import Monadas 
import ErrorMonad
import OutputMonad

data OutputError a = Throwp String | Okp (a, Screen) deriving Show

instance Monad OutputError where 
  return x = Okp (x, "")
  m >>= k = case m of 
              Throwp s -> Throwp s
              Okp v -> let Okp (v, scr1) = m
                        in let Okp (res, scr2) = k v
                          in Okp (res, scr1 ++ scr2)
  fail msg = Throwp msg

instance PrintMonad OutputError where
  printf msg = Okp ((), msg ++ "\n")

instance ErrorMonad OutputError where
  throw msg = Throwp msg


