module Close
where

import Ticket

close :: [String] -> IO ()
close ticks = do
  putStrLn $ "close: " ++ show ticks ++ "."
  froms <- mapM loadTicket ticks
  let froms' = map (setOpen False) froms
  mapM_ saveTicket froms'

handleClose args = close args

