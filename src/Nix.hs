module Main
where

import Data.List (intercalate)
import System (getArgs, getProgName)
import Prelude hiding (catch)
import Text.Printf (printf)

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

commands :: [(String, (String, [String] -> IO ()))]
commands = 
   [("config",  ("setup global nix configuration", handleConfig))
  , ("init",    ("initialize nix in current directory", handleInit))
  , ("add",     ("add a ticket", handleAdd))
  , ("remove",  ("remove a ticket", handleRemove))
  , ("dep",     ("add a dependency between tickets", handleDep))
  , ("comment", ("add a comment to a ticket", handleComment))
  , ("set",     ("set a property of a ticket", handleSetProp))
  , ("tag",     ("tag a ticket", handleTag))
  , ("close",   ("close a ticket", handleClose))
  , ("list",    ("list tickets", handleList))]

usage :: IO ()
usage = do
  n <- getProgName
  putStrLn $ intercalate "\n" $
   [ n ++ " cmd [args]",
     "available commands:" ] ++
     map (\(c, (h, _)) -> printf "    %-12s %s" c h) commands

main = do
  args <- getArgs
  let mcmd = if null args
               then Nothing
               else lookup (head args) commands
  case mcmd of
    Nothing     -> usage
    Just (_, h) -> h (tail args)

