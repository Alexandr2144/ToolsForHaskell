module Parser.Arrows where

import Parser.Class
import Control.Arrow

import Control.Monad.Status


lexeme :: ParserArrow String
lexeme = Kleisli _lexeme where
    _lexeme s = case lex s of
        [(a,s)] -> Success (a,s)
        _ -> exception "lexeme not found"

line :: ParserArrow String
line = Kleisli _line where
    _line s = Success $ break (== '\n') s
