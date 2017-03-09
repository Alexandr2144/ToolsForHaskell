module Config.Extension.Var where

import qualified Config.Extension.Var.Assign as Assign
import qualified Data.Map.Strict as Map

import Config.Extension.Var.Manifest

import Control.Monad.Status
import Control.Arrow

import Parser.Arrows
import Parser.Class

import Data.Log



type Map = Map.Map
type VarsMap = Map String Var
newtype Vars = Vars VarsMap


instance Parser Var where
    parse = lexeme >>> first (arr var) >>> extractVar where
        extractVar = Kleisli _extractVar where
            _extractVar (a,s) = runKleisli a s
        
        
modifyVar :: SafeArrow (Vars, (String, (Assign.Type, (String, String)))) (Vars, String)
modifyVar = arr accuracy >>> first (Kleisli _modifyVar) where
    accuracy (v, (a, (b, (c, s)))) = ((v, a, b, c), s)
    
    _modifyVar :: (Vars, String, Assign.Type, String) -> Status Log Vars
    _modifyVar (v, name, Assign.SetType, val) = setType (v,name) val
    _modifyVar (v, name, Assign.Append, val) = addValue (v,name) val
    _modifyVar (v, name, Assign.Force, val) = setValue (v,name) val
    _modifyVar (v, name, Assign.Lazy, val) = initValue (v,name) val
    

variable :: SafeArrow (Vars,String) (Vars,String)
variable = lvalue >>> assign >>> rvalue >>> modifyVar where
    lvalue = second lexeme
    assign = second $ second parse
    rvalue = second $ second $ second line
    
    
    
setType :: (Vars,String) -> String -> Status Log Vars
setType (m,name) val = (check m) >> (runParser val) >>= saveVar m where
    saveVar (Vars m) v = Success . Vars $ Map.insert name v m
    
    check (Vars m) = if Map.member name m
               then warning ("value" ++ name ++ "already exist")
               else return ()
    
addValue :: (Vars,String) -> String -> Status Log Vars
addValue = undefined

setValue :: (Vars,String) -> String -> Status Log Vars
setValue = undefined

initValue :: (Vars,String) -> String -> Status Log Vars
initValue = undefined


