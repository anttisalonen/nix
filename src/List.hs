module List
where

import System.Locale
import Text.Printf
import Data.List
import Data.Time
import Data.Function

import qualified Data.Edison.Assoc.AssocList as M

import Ticket

list :: IO ()
list = allTickets >>= putStr . displayMany . sortBy (compare `on` title)

handleList _ = list

displayOpen :: Bool -> String
displayOpen True  = "open"
displayOpen False = "closed"

displayCategories :: M.FM String String -> String
displayCategories = intercalate ":" . M.elements . M.mapWithKey (\k a -> printf "%s-%s" k a)

displayMany :: [Ticket] -> String
displayMany ts = "---\n" ++ intercalate "---\n" (map display ts)

displayComments :: [Comment] -> String
displayComments = concatMap (\(c, a, z) -> printf "comment (%s %s):\n%s\n" a (displayTime z) c)

displayTime :: ZonedTime -> String
displayTime = formatTime defaultTimeLocale rfc822DateFormat

display :: Ticket -> String
display t = intercalate "\n"
  [ printf "%s (%s %s)" (title t) (displayTime $ createtime t) (creator t)
  , displayOpen (opened t)
  , displayCategories (categories t) 
  , message t
  , displayComments (comments t) ]

