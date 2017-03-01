module Dialog.Edit where
import Data.List

import qualified System.Process as Proc
import qualified Dialog.Class as Class

   
backspace = reverse.(drop 1).reverse


data Form = Form Mode String String
data Mode = Default | Secret

instance Show Form where
    show (Form Default header value) = header ++ ": " ++ value
    show (Form Secret header value) = header ++ ": " ++ stars
        where stars = take (length value) $ iterate id '*'
    

newtype Popup = Popup (Form -> IO String)
instance Class.Popup Popup where
    popup = Popup present where
        present form = do
            Proc.system "clear"
            putStr (show form)
            ch <- getChar
            case ch of
                 '\n' -> return (value form)
                 '\DEL' -> (unput form) >>= present
                 '\ESC' -> (clear form) >>= present
                 char -> (put char form) >>= present
                 
        put char (Form a b val) = return $ Form a b (val ++ [char])
        unput (Form a b val) = return $ Form a b (backspace val)
        clear (Form a b val) = return $ Form a b ""
        value (Form _ _ val) = val

        
