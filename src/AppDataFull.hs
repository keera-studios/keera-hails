module AppDataFull where

import AppDataBasic (HailsAction)

-- This is the internal action definition. It should be a more
-- complete version of the basic data. Some values can be read from
-- existing files, for instance, from cabal files, from Haskell
-- source, etc.
data AppDataFull = AppDataFull
 { action    :: HailsAction
 , outputDir :: FilePath
 }
 deriving (Show)

