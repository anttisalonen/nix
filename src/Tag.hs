module Tag
where

import Common
import Ticket

tag :: String -> [String] -> IO ()
tag tick tgs = do
  putStrLn $ "tag to: " ++ tick ++ " - " ++ (show tgs) ++ "."
  from <- loadTicket tick
  saveTicket (addTags tgs from)

handleTag args = paramList tag args "tag" "Usage: tag ticket-name tag"

