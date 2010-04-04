module Ticket
where

import Control.Monad
import System.Directory
import System.IO.Error (mkIOError, userErrorType)
import Control.Exception (throwIO)
import Data.Time
import System.FilePath

import qualified Data.Edison.Assoc.AssocList as M

import Global
import Common
import Utils

ticketFilePath :: String -> String
ticketFilePath t = nixdirname </> t

type Comment = (String, String, ZonedTime)

data Ticket = Ticket { title      :: String
                     , message    :: String
                     , opened     :: Bool
                     , deps       :: [String]
                     , comments   :: [Comment]
                     , tags       :: [String]
                     , createtime :: ZonedTime
                     , creator    :: String
                     , categories :: M.FM String String }
  deriving (Show, Read)

modDeps :: ([String] -> [String]) -> Ticket -> Ticket
modDeps f t = t{deps = f (deps t)}

addDep :: String -> Ticket -> Ticket
addDep d t = t{deps = (d:(deps t))}

addComment :: String -> Ticket -> IO Ticket
addComment d = addComments [d]

addComments :: [String] -> Ticket -> IO Ticket
addComments ds t = do
  ct <- currTime
  auth <- getUserName
  return $ t{comments = ((zip3 ds (repeat auth) (repeat ct)) ++ comments t)}

currTime :: IO ZonedTime
currTime = getZonedTime

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

newTicket :: String -> String -> IO Ticket
newTicket t m = do
  crtime <- currTime
  author <- getUserName
  return $ Ticket t m True [] [] [] crtime author (M.empty)

-- overwrites.
saveTicket :: Ticket -> IO ()
saveTicket t =
  writeFile (ticketFilePath (title t)) (show t)

loadTicket :: String -> IO Ticket
loadTicket = loadTicketFile . ticketFilePath

deleteTicket :: String -> IO Bool
deleteTicket t = do
  ex <- ticketExists t
  when ex (removeFile (ticketFilePath t))
  return ex

loadTicketFile :: FilePath -> IO Ticket
loadTicketFile f = do
  contents <- readFileStrict f
  case safeRead contents of
    Nothing -> throwIO $ 
      mkIOError userErrorType
       ("Could not read ticket - file corrupted.")
       Nothing (Just f)
    Just lt -> return lt

allTickets :: IO [Ticket]
allTickets = do
  fs <- getDirectoryContents nixdirname >>= filterM (\f -> isFile (nixdirname </> f))
  mapM loadTicket fs


