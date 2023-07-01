module Monadas where
class Monad m => ErrorMonad m where
    throw :: String -> m a
class Monad m => PrintMonad m where
    printf :: String -> m ()