module Dep
where

import Helpers
import Ticket

dep :: Ticket -> [String] -> IO ()
dep from totitles = do
  putStrLn $ "Dep from " ++ (title from) ++ " to " ++ (show totitles) ++ "."
  mapM_ checkTicketExists totitles
  saveTicket (modDeps (totitles ++) from)

handleDep args = paramList dep args "dep" "Usage: dep ticketname depends-on ..."

