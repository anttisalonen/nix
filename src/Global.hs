module Global
where

import System.Directory
import Control.Exception (throwIO)
import System.IO.Error (mkIOError, doesNotExistErrorType)
import Prelude hiding (catch)

import Utils

appName = "nix"

globalconfigfile :: IO String
globalconfigfile = do
  let globalconfigfilebase = "config"
  appdir <- getAppUserDataDirectory appName
  createDirectoryIfMissing False appdir
  return $ appdir ++ "/" ++ globalconfigfilebase

setGlobalName :: String -> IO ()
setGlobalName n = do
  fpath <- globalconfigfile
  writeFile fpath n

-- throws if not found
readGlobalName :: IO String
readGlobalName = 
  do fpath <- globalconfigfile
     exists <- doesDirectoryExist fpath

     if exists
       then do
         contents <- readFileStrict fpath
         case safeRead contents of
           Just n  -> return n
           Nothing -> flipOut fpath
       else flipOut fpath

  where flipOut fpath = throwIO $ 
          mkIOError doesNotExistErrorType 
             ("Could not read global configuration file - run nix config first.") 
             Nothing (Just fpath)


