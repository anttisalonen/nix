module Main
where

import Data.List (intercalate)
import Control.Monad (liftM)
import System.Directory
import System (getArgs, getProgName)
import System.Console.GetOpt
import Control.Exception (throwIO)
import System.IO.Error (mkIOError, doesNotExistErrorType)
import Prelude hiding (catch)
import qualified Data.ByteString.Char8 as Str

safeRead :: (Read a) => String -> Maybe a
safeRead s = case reads s of
              [(n, _)] -> Just n
              _        -> Nothing

data Command = Config
             | Init
             | Add
             | Dep
             | Comment
             | SetProp
             | Tag
             | Close
             | List
  deriving Eq

commands :: [(String, Command)]
commands = 
   [("config",  Config)
  , ("init",    Init)
  , ("add",     Add)
  , ("dep",     Dep)
  , ("comment", Comment)
  , ("set",     SetProp)
  , ("tag",     Tag)
  , ("close",   Close)
  , ("list",    List)]

commandHandlers :: [(Command, [String] -> IO ())]
commandHandlers =
   [(Config, handleConfig)
  , (Init, handleInit)
  , (Add, handleAdd)
  , (Dep, handleDep)
  , (Comment, handleComment)
  , (SetProp, handleSetProp)
  , (Tag, handleTag)
  , (Close, handleClose)
  , (List, handleList)]

configOptions :: [(OptDescr (ConfigOptions -> ConfigOptions))]
configOptions = [
    Option ['n'] ["name"] (ReqArg setConfigName "name") "your name"
  ]

data ConfigOptions = ConfigOptions { configName :: String }

setConfigName :: String -> ConfigOptions -> ConfigOptions
setConfigName s c = c{configName = s}

defaultConfigOptions = ConfigOptions "unknown"

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
     exists <- doesFileExist fpath

     if exists
       then do
         contents <- liftM Str.unpack $ Str.readFile fpath
         case safeRead contents of
           Just n  -> return n
           Nothing -> flipOut fpath
       else flipOut fpath

  where flipOut fpath = throwIO $ 
          mkIOError doesNotExistErrorType 
             ("Could not read global configuration file - run nix init first.") 
             Nothing (Just fpath)

handleConfig args = do
  let (actions, nonOpts, msgs) = getOpt Permute configOptions args
  if null nonOpts && null msgs
    then do
      let opts = foldl (flip ($)) defaultConfigOptions actions
      putStrLn $ "Your name: " ++ (configName opts)
      setGlobalName (configName opts)
    else do
      mapM_ putStrLn msgs
      putStrLn $ usageInfo "nix config" configOptions 
  
handleInit = error "init not supported yet"
handleAdd = error "add not supported yet"
handleDep = error "dep not supported yet"
handleComment = error "comment not supported yet"
handleSetProp = error "set not supported yet"
handleTag = error "tag not supported yet"
handleClose = error "close not supported yet"
handleList = error "list not supported yet"

usage :: IO ()
usage = do
  n <- getProgName
  putStrLn $ intercalate "\n" $
   [ n ++ " cmd [args]",
     "available commands:" ] ++
     map fst commands

main = do
  args <- getArgs
  let mcmd = if null args
               then Nothing
               else lookup (head args) commands
  case mcmd of
    Nothing -> usage
    Just c  -> do
      case lookup c commandHandlers of
        Just h  -> h (tail args)
        Nothing -> error "No handler setup"

