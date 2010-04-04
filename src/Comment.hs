module Comment
where

import Helpers
import Ticket

comment :: Ticket -> [String] -> IO ()
comment tick comms = do
  putStrLn $ "Comment to: " ++ title tick ++ " - " ++ (show comms) ++ "."
  addComments comms tick >>= saveTicket

handleComment args = paramList comment args "comment" "Usage: comment ticket-name comment"

