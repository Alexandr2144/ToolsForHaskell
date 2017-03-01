module Dialog.List where
import Data.List

import qualified System.Process as Proc
import qualified Dialog.Prompt as Prompt
import qualified Dialog.Class as Class

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing


data Entry = Entry (Int,String)
data StdForm = StdForm Int String [String]
data ComboForm = ComboForm (Prompt.Form) String [String]


instance Show Entry where
    show (Entry (n,line)) = (show n) ++ ". " ++ line
    
instance Show StdForm where
    show (StdForm n header list) = header ++ ":\n" ++ (intercalate "\n" entries)
        where entries = map entry (zip [1..] list)
              entry e@(m,line) = (select m) ++ (show $ Entry e)
              select m = (if m == n then ">> " else "   ")
                  
instance Show ComboForm where
    show (ComboForm prompt header list) = ""
            ++ header ++ ":\n" ++ (intercalate "\n" entries)
            ++ "\n\n" ++ (show prompt)
        where entries = map entry (zip [1..] list)
              entry e = (show $ Entry e)
              

newtype StdPopup = StdPopup (StdForm -> IO String)
newtype ComboPopup = ComboPopup (ComboForm -> IO String)

instance Class.Popup StdPopup where
    popup = StdPopup present where
        present form = draw form >> process form
        
        draw form = do
            Proc.system "clear"
            putStr (show form)
        process form = do
            char <- getChar
            case char of
                 '\n' -> return (value form)
                 'w' -> (return $ up form) >>= present
                 's' -> (return $ down form) >>= present
                 _ -> process form

        value (StdForm n _ list) = list !! (n - 1)
        down form@(StdForm n h list)
            | n == (length list) = form
            | otherwise = StdForm (n+1) h list
        up form@(StdForm 1 _ list) = form
        up (StdForm n h list) = StdForm (n-1) h list
                 
data Cmd = CmdInt Int | CmdStr String

instance Class.Popup ComboPopup where
    popup = ComboPopup present where
        present form = draw form >> process form
    
        draw form = do
            Proc.system "clear"
            putStr (show form)
        process form = do
            raw_cmd <- getLine
            let cmd = readCmd raw_cmd
            return (value cmd form)
        readCmd raw = case ((readMaybe raw) :: Maybe Int) of
            Nothing -> CmdStr raw
            Just a -> CmdInt a

        value (CmdInt n) (ComboForm _ _ list) = list !! (n - 1)
        value (CmdStr s) (ComboForm _ _ list)
            | elem s list = s
            | otherwise = ""
