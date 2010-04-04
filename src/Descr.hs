module Descr
where

import Common
import Ticket

descript :: String -> [String] -> IO ()
descript tick tgs = do
  putStrLn $ "change description of: " ++ tick ++ " - " ++ (head tgs) ++ "."
  t <- loadTicket tick
  saveTicket (setDescr (head tgs) t)

handleDescr args = paramList descript args "descr" "Usage: descr ticket-name new-description"

