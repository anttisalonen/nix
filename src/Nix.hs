module Main
where

import Data.List (intercalate)
import System (getArgs, getProgName)

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

commandHandlers :: [(Command, IO ())]
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

handleConfig = error "config not supported yet"
handleInit = error "init not supported yet"
handleAdd = error "add not supported yet"
handleDep = error "dep not supported yet"
handleComment = error "comment not supported yet"
handleSetProp = error "set not supported yet"
handleTag = error "tag not supported yet"
handleClose = error "close not supported yet"
handleList = error "list not supported yet"

getCmd :: IO (Maybe Command)
getCmd = do
  args <- getArgs
  return $ if null args
             then Nothing
             else lookup (args !! 0) commands

doCmd :: Command -> IO ()
doCmd c = 
  case lookup c commandHandlers of
    Just h  -> h
    Nothing -> error "No handler setup"

-- won't return
usage :: IO ()
usage = do
  n <- getProgName
  putStrLn $ intercalate "\n" $
   [ n ++ " cmd [args]",
     "available commands:" ] ++
     map fst commands

main = do
  mcmd <- getCmd
  case mcmd of
    Nothing -> usage
    Just c  -> doCmd c
