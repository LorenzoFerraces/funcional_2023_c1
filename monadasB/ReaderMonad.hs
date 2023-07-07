{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ReaderMonad where
  
import Monadasb

data Reader r a = R (r -> a)

-- cambiar a version vieja de ghc para hacer funcionar
instance Monad (Reader r) where
  return x = R (\r -> x)
  m >>= k = R (\r -> let R fm = m
                      in let R fk = k (fm r)
                        in fk r)

instance ReaderMonad r (Reader r) where
  ask = R (\r -> r)
  runRM m r = let R fm = m
                in fm r

