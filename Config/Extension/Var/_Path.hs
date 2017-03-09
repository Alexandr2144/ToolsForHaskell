module Config.Type.Path where

import Shell


toLeft [] = Right []
toLeft (x:xs) = Left [x]

toRight [] = Left []
toRight (x:xs) = Right [x]

data Data = Data (IO String)

instance Read Data where
    readsPrec _ s =
        let content = (path >> cmd)
            path = toLeft.(reads :: ReadS String)
            cmd = toRight.(reads :: ReadS Command)
        in case content s of
            Right [(a,b)] -> [(Data $ shell a, b)]
            Left [(a,b)] -> [(Data $ return a, b)]
