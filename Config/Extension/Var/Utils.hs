module Config.Extension.Var.Utils where

import Control.Arrow
import Control.Monad.Status

import Data.Log


type SafeArrow = Kleisli (Status Log)

typeAs :: (a -> b -> c) -> SafeArrow (a, b, String) (c, String)
typeAs f = Kleisli _typeAs where
    _typeAs (a,b,s) = Success (f a b, s) 
