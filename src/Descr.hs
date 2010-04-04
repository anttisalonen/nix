module Descr
where

import Helpers
import Ticket

descript :: Ticket -> [String] -> IO ()
descript tick tgs = do
  putStrLn $ "change description of: " ++ title tick ++ " - " ++ (head tgs) ++ "."
  saveTicket (setDescr (head tgs) tick)

handleDescr args = paramList descript args "descr" "Usage: descr ticket-name new-description"

