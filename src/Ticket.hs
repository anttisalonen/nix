module Ticket
where

import System.Directory
import System.IO.Error (mkIOError, userErrorType)
import Control.Exception (throwIO)

import qualified Data.Edison.Assoc.AssocList as M

import Common
import Utils

ticketFilePath :: String -> String
ticketFilePath t = nixdirname ++ "/" ++ t

data Ticket = Ticket { title      :: String
                     , message    :: String
                     , opened     :: Bool
                     , deps       :: [String]
                     , comments   :: [String]
                     , tags       :: [String]
                     , categories :: M.FM String String }
  deriving (Show, Read)

modDeps :: ([String] -> [String]) -> Ticket -> Ticket
modDeps f t = t{deps = f (deps t)}

addDep :: String -> Ticket -> Ticket
addDep d t = t{deps = (d:(deps t))}

addComment :: String -> Ticket -> Ticket
addComment d t = t{comments = (d:(comments t))}

addComments :: [String] -> Ticket -> Ticket
addComments ds t = t{comments = (ds ++ comments t)}

addTag :: String -> Ticket -> Ticket
addTag d t = t{tags = (d:(tags t))}

addTags :: [String] -> Ticket -> Ticket
addTags ds t = t{tags = (ds ++ tags t)}

addCategory :: String -> String -> Ticket -> Ticket
addCategory k a t = t{categories = M.insert k a (categories t)}

addCategories :: [(String, String)] -> Ticket -> Ticket
addCategories vs t = t{categories = M.union (M.fromSeq vs) (categories t)}

ticketExists :: String -> IO Bool
ticketExists t = doesFileExist (ticketFilePath t)

checkTicketExists :: String -> IO ()
checkTicketExists f = checkFileExists (ticketFilePath f)

setOpen :: Bool -> Ticket -> Ticket
setOpen v t = t{opened = v}

newTicket :: String -> String -> Ticket
newTicket t m = Ticket t m True [] [] [] (M.empty)

-- overwrites.
saveTicket :: Ticket -> IO ()
saveTicket t =
  writeFile (ticketFilePath (title t)) (show t)

loadTicket :: String -> IO Ticket
loadTicket t = do
  contents <- readFileStrict (ticketFilePath t)
  case safeRead contents of
    Nothing -> throwIO $ 
      mkIOError userErrorType
       ("Could not read ticket - file corrupted.")
       Nothing (Just t)
    Just lt -> return lt

