module Main
where

import Data.List (intercalate)
import System (getArgs, getProgName)
import Prelude hiding (catch)

import Add
import Config
import Common
import Dep
import Comment
import Tag
import SetProp
import Close
import List
import Remove

commands :: [(String, [String] -> IO ())]
commands = 
   [("config",  handleConfig)
  , ("init",    handleInit)
  , ("add",     handleAdd)
  , ("remove",  handleRemove)
  , ("dep",     handleDep)
  , ("comment", handleComment)
  , ("set",     handleSetProp)
  , ("tag",     handleTag)
  , ("close",   handleClose)
  , ("list",    handleList)]

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
    Just h  -> h (tail args)

