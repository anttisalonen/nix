module Utils
where

safeRead :: (Read a) => String -> Maybe a
safeRead s = case reads s of
              [(n, _)] -> Just n
              _        -> Nothing


