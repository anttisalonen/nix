module Utils
where

import qualified Data.ByteString.Char8 as Str
import Control.Monad (liftM)

safeRead :: (Read a) => String -> Maybe a
safeRead s = case reads s of
              [(n, _)] -> Just n
              _        -> Nothing


readFileStrict :: String -> IO String
readFileStrict = liftM Str.unpack . Str.readFile

