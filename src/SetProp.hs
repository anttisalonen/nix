module SetProp
where

import Common
import Ticket

toTwo :: [a] -> [(a, a)]
toTwo (a:b:xs) = (a,b):toTwo xs
toTwo _        = []

setProp :: String -> [String] -> IO ()
setProp tick vals = do
  putStrLn $ "tag to: " ++ tick ++ " - " ++ (show vals) ++ "."
  from <- loadTicket tick
  saveTicket (addCategories (toTwo vals) from)

handleSetProp args = paramList setProp args "set" "Usage: set ticket-name category-class category"

