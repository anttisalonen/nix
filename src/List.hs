module List
where

import Ticket

list :: IO ()
list = allTickets >>= putStr . displayMany

handleList _ = list

