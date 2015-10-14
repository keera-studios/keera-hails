module Data.String.Extra
 (trim)
 where

-- | Auxiliary string functions. I can't believe no module declares these
-- FIXME: Check that no existing module declares these.
trim :: String -> String
trim s = reverse (trimBeginning (reverse (trimBeginning s)))

trimBeginning :: String -> String
trimBeginning (' ':xs) = trimBeginning xs
trimBeginning xs       = xs
