-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Model.ReactiveModel.ModelEvents
  ( ModelEvent (FilenameChanged)
  ) where

import qualified Hails.MVC.Model.ReactiveModel as GRM

data ModelEvent = UncapturedEvent
                | FilenameChanged
 deriving (Eq,Ord)

instance GRM.Event ModelEvent where
  undoStackChangedEvent = UncapturedEvent
