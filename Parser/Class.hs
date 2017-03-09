module Parser.Class where
    
import Control.Monad.Status
import Control.Arrow

import Data.Log


type SafeArrow = Kleisli (Status Log)
type ParserArrow a = SafeArrow String (a, String)

class Parser a where
    parse :: SafeArrow String (a, String)

    
runParser :: Parser a => String -> Status Log a
runParser s = runKleisli (parse >>> (arr fst)) s
