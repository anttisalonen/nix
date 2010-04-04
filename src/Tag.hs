module Tag
where

import Helpers
import Ticket

tag :: Ticket -> [String] -> IO ()
tag tick tgs = do
  putStrLn $ "tag to: " ++ title tick ++ " - " ++ (show tgs) ++ "."
  saveTicket (addTags tgs tick)

handleTag args = paramList tag args "tag" "Usage: tag ticket-name tag"

