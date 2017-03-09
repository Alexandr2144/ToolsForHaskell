module Data.Log where

import Control.Monad.Status


newtype Log = Log String
instance Show Log where
    show (Log s) = s
instance Monoid Log where
    mempty = Log mempty
    mappend (Log a) (Log b) = Log $ mappend a b
    
emptyLog header = Log $ header ++ ":\n"

instance StatusLog Log where
    warning s = Warning (Log msg) () where
        msg = "  Warning: " ++ s ++ "\n"
    
    exception s = Exception (Log msg) where
        msg = "  Exception: " ++ s ++ "\n"
