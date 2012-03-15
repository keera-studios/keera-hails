{-# LANGUAGE DeriveDataTypeable #-}
module AppDataBasic where

import Data.Data
import Data.Default
-- import Data.Typeable

-- This is the CLI app definition : what we get from the user
data AppDataBasic = AppDataBasic {
    action    :: HailsAction
  , outputDir :: Maybe FilePath
  , overwrite :: Bool
  }
 deriving (Show, Data, Typeable)

data HailsAction = HailsInit
                 | HailsClean
 deriving (Show, Data, Typeable)
          
instance Default HailsAction where          
  def = HailsInit
