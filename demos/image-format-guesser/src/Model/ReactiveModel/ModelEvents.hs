-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Model.ReactiveModel.ModelEvents
  ( ModelEvent (FilenameChanged)
  ) where

-- import GenericModel.GenericModelEvent
import qualified Control.Concurrent.Model.ReactiveModel as GRM

data ModelEvent = UncapturedEvent
                | FilenameChanged
 deriving (Eq,Ord)

instance GRM.Event ModelEvent where
  undoStackChangedEvent = UncapturedEvent
