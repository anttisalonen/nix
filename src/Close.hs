module Close
where

import Ticket
import Helpers

close :: [Ticket] -> IO ()
close ticks = do
  putStrLn $ "close: " ++ show (map title ticks) ++ "."
  let ticks' = map (setOpen False) ticks
  mapM_ saveTicket ticks'

handleClose args = do
  ts <- loadArgsAsTickets args "close ticketname ... - closes tickets"
  close ts

