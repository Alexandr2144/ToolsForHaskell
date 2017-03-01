module Dialog.Inline where
import Dialog.Class
import Data.List


data Prompt = Prompt [String]
data Ask = Ask String

newtype Info = Info String
newtype Message = Message String


-- Show impl --
instance Show Prompt where
    show (Prompt path) = (intercalate "/" path) ++ "> "
   
instance Show Message where
    show (Message txt) = txt
    
instance Show Info where
    show (Info txt) = txt
    
instance Show Ask where
    show (Ask txt) = txt ++ " (y/n)> "
         


newtype PromptPopup = PromptPopup (Prompt -> IO String)


instance Popup PromptPopup where
    popup = PromptPopup impl
        where impl prompt = putStr (show prompt) >> getLine
