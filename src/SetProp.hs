module SetProp
where

import Helpers
import Ticket

toTwo :: [a] -> [(a, a)]
toTwo (a:b:xs) = (a,b):toTwo xs
toTwo _        = []

setProp :: Ticket -> [String] -> IO ()
setProp tick vals = do
  putStrLn $ "property to: " ++ title tick ++ " - " ++ (show vals) ++ "."
  saveTicket (addCategories (toTwo vals) tick)

handleSetProp args = paramList setProp args "set" "Usage: set ticket-name category-class category"

