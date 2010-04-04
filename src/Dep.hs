module Dep
where

import Helpers
import Ticket

dep :: Ticket -> [String] -> IO ()
dep from totitles = do
  fs <- mapM expandTicketGlob totitles
  ticks <- checkAllRight fs usagemsg
  putStrLn $ "Dep from " ++ (title from) ++ " to " ++ (show ticks) ++ "."
  saveTicket (modDeps (ticks ++) from)

usagemsg = "Usage: dep ticketname depends-on ..."

handleDep args = paramList dep args "dep" usagemsg

