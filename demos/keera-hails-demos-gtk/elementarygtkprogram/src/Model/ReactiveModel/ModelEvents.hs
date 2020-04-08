module Model.ReactiveModel.ModelEvents
  ( ModelEvent ()
  ) where

-- import GenericModel.GenericModelEvent
import qualified Hails.MVC.Model.ReactiveModel as GRM

data ModelEvent = UncapturedEvent
 deriving (Eq,Ord)

instance GRM.Event ModelEvent where
  undoStackChangedEvent = UncapturedEvent
