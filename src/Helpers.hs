module Helpers
where

import System.Exit
import Data.Either

import Ticket
import Common

-- checks that the first item in the parameter is a valid ticket.
-- then calls the given function with the ticket and the rest of
-- the arguments.
paramList
  :: (Ticket -> [String] -> IO b)
     -> [String]
     -> String
     -> String
     -> IO b
paramList f args cmd usagemsg = do
  checkIsInit
  (_, nonopts) <- doArgs [] undefined [] cmd args True
  if length nonopts < 2
    then putStrLn usagemsg >> exitWith (ExitFailure 1)
    else do
      mtick <- findTicket (head nonopts)
      case mtick of
        Right t -> f t (tail nonopts)
        Left  e -> do
          putStrLn e
          putStrLn usagemsg
          exitWith (ExitFailure 1)

-- checks that all arguments are valid tickets.
-- then loads these tickets and returns them.
loadArgsAsTickets :: [String] -> String -> IO [Ticket]
loadArgsAsTickets args usagemsg = do
  checkIsInit
  if null args
    then putStrLn usagemsg >> exitWith (ExitFailure 1)
    else do
      ts <- mapM findTicket args
      checkAllRight ts usagemsg

-- exits on failure.
checkAllRight :: [Either String b] -> String -> IO [b]
checkAllRight es usagemsg = do
  let ls = lefts es
  if null ls
    then return (rights es)
    else do
      mapM_ putStrLn ls
      putStrLn usagemsg
      exitWith (ExitFailure 1)

