module Dep
where

import Common
import Ticket

dep :: String -> [String] -> IO ()
dep fromtitle totitles = do
  putStrLn $ "Dep from " ++ fromtitle ++ " to " ++ (show totitles) ++ "."
  mapM_ checkTicketExists totitles
  from <- loadTicket fromtitle
  saveTicket (modDeps (totitles ++) from)

handleDep args = paramList dep args "dep" "Usage: dep ticketname depends-on ..."

