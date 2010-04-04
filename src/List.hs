module List
where

import System.Console.GetOpt
import Text.Printf
import Data.List
import Data.Time
import Data.Function

import qualified Data.Edison.Assoc.AssocList as M

import Ticket
import Common

type TicketFilter = Ticket -> Bool
type TicketSorting = Ticket -> Ticket -> Ordering
type TicketShow = [Ticket] -> String

data ListOptions = ListOptions { filters    :: [TicketFilter]
                               , sorting    :: TicketSorting
                               , reversed   :: Bool
                               , formatting :: TicketShow }

addFilter :: TicketFilter -> ListOptions -> ListOptions
addFilter s c = c{filters = (s:filters c)}

setSorting :: TicketSorting -> ListOptions -> ListOptions
setSorting s c = c{sorting = s}

setReversed :: Bool -> ListOptions -> ListOptions
setReversed s c = c{reversed = s}

setFormatting :: TicketShow -> ListOptions -> ListOptions
setFormatting s c = c{formatting = s}

getCategories :: String -> ListOptions -> ListOptions
getCategories n = 
  let (c, v') = span (/= '=') n
  in addFilter (\t -> getCategoryValue c t == (drop 1 v'))

getTag :: String -> ListOptions -> ListOptions
getTag n = addFilter (hasTag n)

defaultListOptions = ListOptions [] (compare `on` title) False displayMany

listOptions :: [(OptDescr (ListOptions -> ListOptions))]
listOptions = [
    Option ['r'] ["reverse"]  (NoArg  (setReversed True))                     "reverse ordering"
  , Option ['c'] ["closed"]   (NoArg  (addFilter (not . opened)))             "only include closed tickets"
  , Option ['o'] ["open"]     (NoArg  (addFilter opened))                     "only include open tickets"
  , Option ['d'] ["date"]     (NoArg  (setSorting (compare `on` createtime))) "sort on creation time"
  , Option ['a'] ["alpha"]    (NoArg  (setSorting (compare `on` title)))      "sort on titles (alphabetically) (default)"
  , Option ['C'] ["category"] (ReqArg getCategories "cat=val")                "filter by category (\"-c cat=val\")"
  , Option ['t'] ["tag"]      (ReqArg getTag "tag")                           "filter by tag"
  , Option ['s'] ["short"]    (NoArg  (setFormatting displayManyShort))       "short formatting of tickets"
  , Option ['l'] ["long"]     (NoArg  (setFormatting displayMany))            "long formatting of tickets (default)"
  ]

handleList args = do
  (opts, _) <- doArgs listOptions defaultListOptions [] "list" args False
  list opts

filterAll :: [a -> Bool] -> [a] -> [a]
filterAll []     xs = xs
filterAll (f:fs) xs = filterAll fs (filter f xs)

list :: ListOptions -> IO ()
list opts = do
  tickets <- allTickets
  let ts = filterAll (filters opts) tickets
  putStrLn . formatting opts . (if reversed opts then reverse else id) . sortBy (sorting opts) $ ts

displayOpen :: Bool -> String
displayOpen True  = "open"
displayOpen False = "closed"

displayCategories :: M.FM String String -> String
displayCategories = intercalate ":" . M.elements . M.mapWithKey (\k a -> printf "%s-%s" k a)

displayTags :: [String] -> String
displayTags = intercalate "+"

displayMany :: TicketShow
displayMany ts = "---\n" ++ intercalate "\n---\n" (map display ts)

displayManyShort :: TicketShow
displayManyShort ts = intercalate "\n" (map title ts)

displayComments :: [Comment] -> String
displayComments = concatMap (\(c, a, z) -> printf "comment (%s %s):\n%s\n" a (displayTime z) c)

displayTime :: UTCTime -> String
displayTime = show

display :: Ticket -> String
display t = intercalate "\n" $ filter (/= "")
  [ printf "%s (%s %s)" (title t) (displayTime $ createtime t) (creator t)
  , displayOpen (opened t)
  , displayCategories (categories t) 
  , displayTags (tags t) 
  , message t
  , displayComments (comments t) ]

