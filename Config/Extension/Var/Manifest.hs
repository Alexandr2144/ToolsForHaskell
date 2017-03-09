module Config.Extension.Var.Manifest where
import Config.Extension.Var.Utils
import Control.Arrow


import qualified Config.Extension.Var.Path as Path



data Var = Undefined
         | Path Path.Value Path.Type
         

var :: String -> SafeArrow String (Var,String)
var "Path" = Path.typeid >>> Path.none >>> typeAs Path

newVar "Path" = Path.typeid >>> Path.none >>> typeAs Path
setVar "Path" = Path.typeid >>> Path.none >>> typeAs Path
