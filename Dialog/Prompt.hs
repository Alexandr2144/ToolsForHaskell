module Dialog.Prompt where
import Data.List

import qualified Dialog.Class as Class


data Form = Form [String]

-- Show impl --
instance Show Form where
    show (Form path) = (intercalate "/" path) ++ "> "

newtype Popup = Popup (Form -> IO String)


instance Class.Popup Popup where
    popup = Popup present where
        present form = putStr (show form) >> getLine
