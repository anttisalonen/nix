module Remove
where

import Control.Monad

import Ticket
import Common

handleRemove args = do
  handleDefaultArgs args "remove ticketname ... - removes tickets" []
  founds <- mapM deleteTicket args
  forM_ (zip args founds) 
    (\(t, f) -> putStrLn $ if f then "Removed ticket \"" ++ t ++ "\"." else "Ticket \"" ++ t ++ "\" not found.")
