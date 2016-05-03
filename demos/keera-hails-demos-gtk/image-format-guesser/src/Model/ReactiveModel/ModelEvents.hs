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
