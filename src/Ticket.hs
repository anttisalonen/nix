module Ticket
where

import Control.Monad
import System.Directory
import System.IO.Error (mkIOError, doesNotExistErrorType, userErrorType)
import Control.Exception (throwIO)
import Data.Time
import System.FilePath
import System.FilePath.Glob

import qualified Data.Edison.Assoc.AssocList as M

import Global
import Common
import Utils

ticketFilePath :: String -> String
ticketFilePath t = nixdirname </> t

type Comment = (String, String, UTCTime)

data Ticket = Ticket { title      :: String
                     , descr      :: String
                     , opened     :: Bool
                     , deps       :: [String]
                     , comments   :: [Comment]
                     , tags       :: [String]
                     , createtime :: UTCTime
                     , creator    :: String
                     , categories :: M.FM String String }
  deriving (Show, Read)

modDeps :: ([String] -> [String]) -> Ticket -> Ticket
modDeps f t = t{deps = f (deps t)}

addDep :: String -> Ticket -> Ticket
addDep d t = t{deps = (d:(deps t))}

setDescr :: String -> Ticket -> Ticket
setDescr d t = t{descr = d}

addComment :: String -> Ticket -> IO Ticket
addComment d = addComments [d]

addComments :: [String] -> Ticket -> IO Ticket
addComments ds t = do
  ct <- currTime
  auth <- getUserName
  return $ t{comments = ((zip3 ds (repeat auth) (repeat ct)) ++ comments t)}

currTime :: IO UTCTime
currTime = getCurrentTime

addTag :: String -> Ticket -> Ticket
addTag d t = t{tags = (d:(tags t))}

addTags :: [String] -> Ticket -> Ticket
addTags ds t = t{tags = (ds ++ tags t)}

addCategory :: String -> String -> Ticket -> Ticket
addCategory k a t = t{categories = M.insert k a (categories t)}

addCategories :: [(String, String)] -> Ticket -> Ticket
addCategories vs t = t{categories = M.union (M.fromSeq vs) (categories t)}

-- returns "" if not found.
getCategoryValue :: String -> Ticket -> String
getCategoryValue c t = M.lookupWithDefault "" c (categories t)

hasTag :: String -> Ticket -> Bool
hasTag s t = s `elem` tags t

ticketExists :: String -> IO Bool
ticketExists t = doesFileExist (ticketFilePath t)

ticketGlobExists :: String -> IO Bool
ticketGlobExists t = do
  ef <- findTicket t
  case ef of
    Left  _ -> return False
    Right _ -> return True

checkTicketExists :: String -> IO ()
checkTicketExists f = checkFileExists (ticketFilePath f)

checkTicketGlobExists :: String -> IO ()
checkTicketGlobExists t = do
  ef <- findTicket t
  case ef of
    Left e  -> throwIO $ 
                mkIOError doesNotExistErrorType 
                   e
                   Nothing (Just t)
    Right _ -> return ()

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

-- throws on failure.
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

findTicket :: String -> IO (Either String Ticket)
findTicket n = do
  fn <- expandTicketGlob n
  case fn of
    Left e  -> return $ Left e
    Right t -> return . Right =<< loadTicket t

expandTicketGlob :: String -> IO (Either String String)
expandTicketGlob t = do
  files <- globDir1 (compile ('*':t ++ "*")) nixdirname
  case files of
    []  -> return $ Left $ "No match found: " ++ t
    [f] -> return $ Right $ takeFileName f
    _   -> return $ Left "Ambiguous pattern used"

