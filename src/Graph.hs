module Graph
where

import System.Console.GetOpt
import Data.Maybe
import Control.Monad

import Data.Graph.Inductive
import qualified Data.Edison.Assoc.StandardMap as M

import Common
import Filter
import Ticket

swap (a, b) = (b, a)

fetch :: M.FM String Int -> (String, String) -> Maybe (Int, Int)
fetch m (s1, s2) = 
  let n1 = M.lookupM s1 m
      n2 = M.lookupM s2 m
  in liftM2 (,) n1 n2

data GraphOptions = GraphOptions { filters    :: [TicketFilter]
                                 , reversed   :: Bool
                                 }

addFilter :: TicketFilter -> GraphOptions -> GraphOptions
addFilter s c = c{filters = (s:filters c)}

setReversed :: Bool -> GraphOptions -> GraphOptions
setReversed s c = c{reversed = s}

defaultGraphOptions = GraphOptions [] False

graphOptions :: [(OptDescr (GraphOptions -> GraphOptions))]
graphOptions = [
    Option ['r'] ["reverse"]  (NoArg  (setReversed True))                     "reverse ordering"
  , Option ['c'] ["closed"]   (NoArg  (addFilter closedF))                    "only include closed tickets"
  , Option ['o'] ["open"]     (NoArg  (addFilter openF))                      "only include open tickets"
  , Option ['C'] ["category"] (ReqArg (getCategories addFilter) "cat=val")    "filter by category (\"-c cat=val\")"
  , Option ['t'] ["tag"]      (ReqArg (getTag addFilter) "tag")               "filter by tag"
  ]

handleGraph args = do
  (opts, _) <- doArgs graphOptions defaultGraphOptions [] "graph" args False
  graph opts

graph opts = do
  alltickets <- (filterAll (filters opts)) `fmap` allTickets
  let mapping1    = M.fromSeq nodelist
      nodelist    = zip (map title alltickets) [(1 :: Int)..]
      depedges    = concatMap (\t -> zip (repeat (title t)) (deps t)) alltickets
      depedgenums = map (fetch mapping1) depedges
      gr :: Gr String ()
      gr          = mkGraph (map swap nodelist) (map (\(n, m) -> (n, m, ())) (catMaybes depedgenums))
  putStrLn "---"
  putStrLn $ showDeps depedges
  putStrLn "---"
  print gr
  putStrLn "---"
  mapM_ putStrLn (topsort' gr)

showDeps = concatMap (\(a, b) -> a ++ " -> " ++ b ++ "\n")

