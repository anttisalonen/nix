module Add
where

import System.Console.GetOpt

import Common
import Ticket

addOptions :: [(OptDescr (AddOptions -> AddOptions))]
addOptions = [
    Option ['m'] ["message"] (ReqArg setMessage "message") "message"
  ]

data AddOptions = AddOptions { addtitle   :: String
                             , addmessage :: String }
  deriving (Show, Read)

setTitle :: String -> AddOptions -> AddOptions
setTitle s c = c{addtitle = s}

setMessage :: String -> AddOptions -> AddOptions
setMessage s c = c{addmessage = s}

defaultAddOptions = AddOptions "" ""

-- TODO: do not overwrite.
add :: AddOptions -> IO ()
add opt = do
  checkIsInit
  saveTicket (newTicket (addtitle opt) (addmessage opt))

handleAdd args = do
  (opts, nonopts) <- doArgs addOptions defaultAddOptions [] "add" args True
  if length nonopts /= 1
    then putStrLn "You must tell me the title of the ticket." >> cmdUsage "add" addOptions
    else do
      putStrLn $ "Adding " ++ (head nonopts) ++ " - " ++ addmessage opts
      add (setTitle (head nonopts) opts)


