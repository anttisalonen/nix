module Remove
where

import Control.Monad

import Ticket

handleRemove args = do
  founds <- mapM deleteTicket args
  forM_ (zip args founds) 
    (\(t, f) -> putStrLn $ if f then "Removed ticket \"" ++ t ++ "\"." else "Ticket \"" ++ t ++ "\" not found.")
