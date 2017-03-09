module Config.Extension.Var.Assign where

import Control.Monad.Status
import Control.Arrow

import Parser.Arrows
import Parser.Class


data Type = SetType | Append | Force | Lazy

instance Parser Type where
    parse = undefined
