module Ticket
where

import System.Directory
import System.IO.Error (mkIOError, userErrorType)
import Control.Exception (throwIO)

import Common
import Utils

ticketFilePath :: String -> String
ticketFilePath t = nixdirname ++ "/" ++ t

data Ticket = Ticket { title   :: String
                     , message :: String
                     , deps    :: [String] }
  deriving (Show, Read)

modDeps :: ([String] -> [String]) -> Ticket -> Ticket
modDeps f t = t{deps = f (deps t)}

ticketExists :: String -> IO Bool
ticketExists t = doesFileExist (ticketFilePath t)

checkTicketExists :: String -> IO ()
checkTicketExists f = checkFileExists (ticketFilePath f)

newTicket :: String -> String -> Ticket
newTicket t m = Ticket t m []

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

