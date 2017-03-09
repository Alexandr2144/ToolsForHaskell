module Shell where

import qualified System.Process as System


data Command = Command String
    deriving Show

shell :: Command -> IO String
shell (Command cmd) = System.readProcess name args "" where
    (name:args) = words cmd

instance Read Command where
    readsPrec _ s =
        let rawCmd = dollar s >>= str
            dollar ('$':xs) = Just xs
            dollar _ = Nothing
            str s = Just (reads s)
        in case rawCmd of
            Just [(s,xs)] -> [(Command s, xs)]
            _ -> []
            
