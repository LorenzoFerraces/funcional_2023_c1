module MaybeMonad where
import Monadas
instance ErrorMonad Maybe where
    throw msg = Nothing