module Main
where

import Data.List (intercalate)
import Control.Monad (when, liftM)
import System.Exit
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

defaultConfigOptions = ConfigOptions ""

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
         contents <- liftM Str.unpack $ Str.readFile fpath
         case safeRead contents of
           Just n  -> return n
           Nothing -> flipOut fpath
       else flipOut fpath

  where flipOut fpath = throwIO $ 
          mkIOError doesNotExistErrorType 
             ("Could not read global configuration file - run nix config first.") 
             Nothing (Just fpath)

handleConfig args = do
  (opts, _) <- doArgs configOptions defaultConfigOptions [(\o -> not (null (configName o)))] "config" args False
  putStrLn $ "Your name: " ++ (configName opts)
  setGlobalName (configName opts)

isvalid :: [a -> Bool] -> a -> [Bool]
isvalid []     _ = []
isvalid (f:fs) v = f v : isvalid fs v

doArgs
  :: [OptDescr (a -> a)] -- optdescr
  -> a                   -- default data
  -> [a -> Bool]         -- validation functions on parsed options
  -> String              -- name of command for error message
  -> [String]            -- cmd line arguments
  -> Bool                -- whether nonopts are allowed
  -> IO (a, [String])    -- parsed data and messages (function exits on failure)
doArgs opts defopts validfuncs cmd args allownonopts = do
  let (actions, nonOpts, msgs) = getOpt Permute opts args
  if (null nonOpts || allownonopts) && null msgs
    then do
      let finalopts = foldl (flip ($)) defopts actions
      if and $ isvalid validfuncs finalopts
        then return (finalopts, nonOpts)
        else cmdUsage cmd opts
    else cmdUsage cmd opts

nixdirname = ".nix"  

handleInit _ = do
  createDirectoryIfMissing False nixdirname
  putStrLn $ "Created directory " ++ nixdirname

addOptions :: [(OptDescr (AddOptions -> AddOptions))]
addOptions = [
    Option ['m'] ["message"] (ReqArg setMessage "message") "message"
  ]

data AddOptions = AddOptions { title :: String
                             , message :: String }
  deriving (Show, Read)

setTitle :: String -> AddOptions -> AddOptions
setTitle s c = c{title = s}

setMessage :: String -> AddOptions -> AddOptions
setMessage s c = c{message = s}

defaultAddOptions = AddOptions "" ""

-- exits.
cmdUsage cmd opts = putStrLn (usageInfo ("nix " ++ cmd) opts) >> exitWith (ExitFailure 1)

-- throws error if not init.
checkIsInit = do
  exists <- doesDirectoryExist nixdirname
  when (not exists) $ throwIO $ 
    mkIOError doesNotExistErrorType 
       ("Could not find nix directory - run nix init first.") 
       Nothing (Just nixdirname)

handleAdd args = do
  (opts, nonopts) <- doArgs addOptions defaultAddOptions [] "add" args True
  if length nonopts /= 1
    then putStrLn "You must tell me the title of the issue." >> cmdUsage "add" addOptions
    else do
      putStrLn $ "Adding " ++ (head nonopts) ++ " - " ++ message opts
      add (setTitle (head nonopts) opts)

issueFilePath :: String -> String
issueFilePath t = nixdirname ++ "/" ++ t

add :: AddOptions -> IO ()
add opt = do
  checkIsInit
  writeFile (issueFilePath (title opt)) (show opt)

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

