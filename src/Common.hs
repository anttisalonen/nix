module Common
where

import System.Console.GetOpt
import System.IO.Error (mkIOError, doesNotExistErrorType)
import Control.Exception (throwIO)
import Control.Monad (when)
import System.Exit
import System.Directory

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

-- exits.
cmdUsage cmd opts = putStrLn (usageInfo ("nix " ++ cmd) opts) >> exitWith (ExitFailure 1)

-- throws error if not init.
checkIsInit = do
  exists <- doesDirectoryExist nixdirname
  when (not exists) $ throwIO $ 
    mkIOError doesNotExistErrorType 
       ("Could not find nix directory - run nix init first.") 
       Nothing (Just nixdirname)

-- throws error if does not exist.
checkFileExists f = do
  exists <- doesFileExist f
  when (not exists) $ throwIO $ 
    mkIOError doesNotExistErrorType 
       ""
       Nothing (Just f)

paramList f args cmd usagemsg = do
  checkIsInit
  (_, nonopts) <- doArgs [] undefined [] cmd args True
  if length nonopts < 2
    then putStrLn usagemsg >> exitWith (ExitFailure 1)
    else f (head nonopts) (tail nonopts)

