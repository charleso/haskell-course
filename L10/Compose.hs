module L10.Compose where

import Control.Applicative

newtype Compose f g a =
  Compose (f (g a))

instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  fmap f (Compose k) =
    Compose (fmap (fmap f) k)

instance (Applicative f, Applicative g) =>
    Applicative (Compose f g) where
  pure = 
    Compose . pure . pure
  Compose f <*> Compose a =
    Compose (liftA2 (<*>) f a)

instance (Monad f, Monad g) =>
    Monad (Compose f g) where
  return = 
    Compose . return . return
  Compose a >>= f =
    undefined -- ?

