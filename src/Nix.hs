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

