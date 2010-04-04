module Comment
where

import Common
import Ticket

comment :: String -> [String] -> IO ()
comment tick comms = do
  putStrLn $ "Comment to: " ++ tick ++ " - " ++ (show comms) ++ "."
  from <- loadTicket tick
  addComments comms from >>= saveTicket

handleComment args = paramList comment args "comment" "Usage: comment ticket-name comment"

