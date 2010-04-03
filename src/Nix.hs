module Main
where

import Data.List (intercalate)
import System (getArgs, getProgName)
import System.Console.GetOpt

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

handleConfig args = do
  let (actions, nonOpts, msgs) = getOpt Permute configOptions args
  if null nonOpts && null msgs
    then do
      let opts = foldl (flip ($)) defaultConfigOptions actions
      putStrLn $ "Your name: " ++ (configName opts)
    else do
      if not (null msgs)
        then mapM_ putStrLn msgs
        else usage
  
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

