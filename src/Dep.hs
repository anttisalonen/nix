module Dep
where

import System.Exit

import Common
import Ticket

dep :: String -> String -> IO ()
dep fromtitle totitle = do
  putStrLn $ "Dep from " ++ fromtitle ++ " to " ++ totitle ++ "."
  checkTicketExists totitle
  from <- loadTicket fromtitle
  saveTicket (modDeps (totitle:) from)

handleDep args = do
  checkIsInit
  (_, nonopts) <- doArgs [] undefined [] "dep" args True
  if length nonopts /= 2
    then putStrLn "Usage: dep dependency-from dependency-to" >> exitWith (ExitFailure 1)
    else dep (head nonopts) (nonopts !! 1)

