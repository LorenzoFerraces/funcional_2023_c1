{-# LANGUAGE FunctionalDependencies #-}
module Monadasb where

class Monad m => ErrorMonad m where
  throw :: String -> m a

class Monad m => PrintMonad m where
  printf :: String -> m ()

class Monad m => ReaderMonad r m | m -> r where
  ask :: m r
  runRM :: m a -> r -> a