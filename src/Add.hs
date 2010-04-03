module Add
where

import System.Console.GetOpt

import Common

addOptions :: [(OptDescr (AddOptions -> AddOptions))]
addOptions = [
    Option ['m'] ["message"] (ReqArg setMessage "message") "message"
  ]

data AddOptions = AddOptions { title :: String
                             , message :: String }
  deriving (Show, Read)

setTitle :: String -> AddOptions -> AddOptions
setTitle s c = c{title = s}

setMessage :: String -> AddOptions -> AddOptions
setMessage s c = c{message = s}

defaultAddOptions = AddOptions "" ""

add :: AddOptions -> IO ()
add opt = do
  checkIsInit
  writeFile (issueFilePath (title opt)) (show opt)

handleAdd args = do
  (opts, nonopts) <- doArgs addOptions defaultAddOptions [] "add" args True
  if length nonopts /= 1
    then putStrLn "You must tell me the title of the issue." >> cmdUsage "add" addOptions
    else do
      putStrLn $ "Adding " ++ (head nonopts) ++ " - " ++ message opts
      add (setTitle (head nonopts) opts)


