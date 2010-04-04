module Helpers
where

import System.Exit
import Data.Either

import Ticket
import Common

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

loadArgsAsTickets :: [String] -> String -> IO [Ticket]
loadArgsAsTickets args usagemsg = do
  checkIsInit
  if null args
    then putStrLn usagemsg >> exitWith (ExitFailure 1)
    else do
      ticks <- mapM findTicket args
      let ls = lefts ticks
      if null ls
        then return (rights ticks)
        else do
          mapM_ putStrLn ls
          putStrLn usagemsg
          exitWith (ExitFailure 1)

