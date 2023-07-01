module ErrorMonad where
import Monadas
data Error a = Throw String | Ok a deriving Show

instance Monad Error where
  return x = Ok x
  m >>= k = case m of
              Throw s -> Throw s
              Ok v -> k v
  fail msg = Throw msg
  
instance ErrorMonad Error where
  throw msg = Throw msg