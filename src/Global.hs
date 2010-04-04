module Global
where

import System.Directory
import Control.Exception (throwIO)
import System.IO.Error (mkIOError, userErrorType, doesNotExistErrorType)
import Prelude hiding (catch)
import System.FilePath

import Utils

appName = "nix"

globalconfigfile :: IO String
globalconfigfile = do
  let globalconfigfilebase = "config"
  appdir <- getAppUserDataDirectory appName
  createDirectoryIfMissing False appdir
  return $ appdir </> globalconfigfilebase

setGlobalName :: String -> IO ()
setGlobalName n = do
  fpath <- globalconfigfile
  writeFile fpath (show n)

-- throws if not found
readGlobalName :: IO String
readGlobalName = 
  do fpath <- globalconfigfile
     exists <- doesFileExist fpath

     if exists
       then do
         contents <- readFileStrict fpath
         case safeRead contents of
           Just n  -> return n
           Nothing -> flipOut userErrorType fpath
       else flipOut doesNotExistErrorType fpath

  where flipOut e fpath = throwIO $ 
          mkIOError e 
             ("Could not read global configuration file - run nix config first.") 
             Nothing (Just fpath)

getUserName = readGlobalName

