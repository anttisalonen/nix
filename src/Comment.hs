module Comment
where

import System.Exit

import Common
import Ticket

comment :: String -> String -> IO ()
comment tick comm = do
  putStrLn $ "Comment to: " ++ tick ++ " - " ++ comm ++ "."
  from <- loadTicket tick
  saveTicket (addComment comm from)

handleComment args = do
  checkIsInit
  (_, nonopts) <- doArgs [] undefined [] "comment" args True
  if length nonopts /= 2
    then putStrLn "Usage: comment ticket-name comment" >> exitWith (ExitFailure 1)
    else comment (head nonopts) (nonopts !! 1)

