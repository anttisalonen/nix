module Remove
where

import Control.Monad

import Ticket
import Helpers

handleRemove args = do
  _ <- loadArgsAsTickets args "remove ticketname ... - removes tickets"
  mapM_ deleteTicket args
  forM_ args
    (\t -> putStrLn $ "Removed ticket \"" ++ t ++ "\".")

