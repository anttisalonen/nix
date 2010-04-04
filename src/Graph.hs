module Graph
where

import Data.Maybe
import Control.Monad

import Data.Graph.Inductive
import qualified Data.Edison.Assoc.StandardMap as M

import Ticket

swap (a, b) = (b, a)

fetch :: M.FM String Int -> (String, String) -> Maybe (Int, Int)
fetch m (s1, s2) = 
  let n1 = M.lookupM s1 m
      n2 = M.lookupM s2 m
  in liftM2 (,) n1 n2

handleGraph _ = do
  alltickets <- allTickets
  let mapping1    = M.fromSeq nodelist
      nodelist    = zip (map title alltickets) [(1 :: Int)..]
      depedges    = concatMap (\t -> zip (repeat (title t)) (deps t)) alltickets
      depedgenums = map (fetch mapping1) depedges
      gr :: Gr String ()
      gr          = mkGraph (map swap nodelist) (map (\(n, m) -> (n, m, ())) (catMaybes depedgenums))
  putStrLn $ showDeps depedges
  print gr
  mapM_ putStrLn (topsort' gr)

showDeps = concatMap (\(a, b) -> a ++ " -> " ++ b ++ "\n")

