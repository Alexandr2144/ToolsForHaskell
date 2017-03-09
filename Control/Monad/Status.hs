module Control.Monad.Status where

import Data.Monoid


data Status e a = Exception e | Warning e a | Success a
    deriving Show

class Monoid e => StatusLog e where
    warning :: String -> Status e ()
    exception :: String -> Status e a

instance Functor (Status e) where
    fmap f (Success a) = Success (f a)
    fmap f (Warning e a) = Warning e (f a)
    fmap f (Exception e) = Exception e

instance Monoid e => Applicative (Status e) where
    pure = Success
    
    (Success f) <*> a = fmap f a

    (Warning e f) <*> (Success a) = Warning e (f a)
    (Warning e f) <*> (Warning e' a) = Warning (mappend e e') (f a)
    (Warning e f) <*> (Exception e') = Exception (mappend e e')
    
    (Exception e) <*> _ = Exception e
    
instance StatusLog e => Monad (Status e) where
    return = Success

    (Success _) >> a = a

    (Warning e _) >> (Success a) = Warning e a
    (Warning e _) >> (Warning e' a) = Warning (mappend e e') a
    (Warning e _) >> (Exception e') = Exception (mappend e e')
    
    (Exception e) >> _ = Exception e

    (>>=) (Success a) f = f a
    (>>=) m@(Warning e a) f = m >> (f a)
    (>>=) (Exception e) f = Exception e
    
    fail s = exception s
    
check :: ((Maybe e, Maybe a) -> Status e b) -> Status e a -> Status e b
check f (Success a) = f (Nothing, Just a)
check f (Warning e a) = f (Just e, Just a)
check f (Exception e) = f (Just e, Nothing)

catch :: (e -> Status e a) -> Status e a -> Status e a
catch f (Exception e) = f(e)
catch f m = m

validate :: (e -> Status e a) -> Status e a -> Status e a
validate f (Warning e a) = f(e)
validate f (Exception e) = f(e)
validate f m = m

