module Utils
where

import qualified Data.ByteString.Char8 as Str
import Control.Monad (liftM)
import System.Directory

safeRead :: (Read a) => String -> Maybe a
safeRead s = case reads s of
              [(n, _)] -> Just n
              _        -> Nothing


readFileStrict :: String -> IO String
readFileStrict = liftM Str.unpack . Str.readFile

isFile :: FilePath -> IO Bool
isFile = doesFileExist

