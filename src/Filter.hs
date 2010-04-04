module Filter
where

import Ticket

type TicketFilter = Ticket -> Bool

closedF :: TicketFilter
closedF = not . opened

openF :: TicketFilter
openF = opened

getCategories :: (TicketFilter -> a -> a) -> String -> a -> a
getCategories addf n = 
  let (c, v') = span (/= '=') n
  in addf (\t -> getCategoryValue c t == (drop 1 v'))

getTag :: (TicketFilter -> a -> a) -> String -> a -> a
getTag addf n = addf (hasTag n)

filterAll :: [a -> Bool] -> [a] -> [a]
filterAll []     xs = xs
filterAll (f:fs) xs = filterAll fs (filter f xs)


