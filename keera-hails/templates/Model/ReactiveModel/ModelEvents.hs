module Model.ReactiveModel.ModelEvents where

import qualified Hails.MVC.Model.ReactiveModel as GRM
import Hails.MVC.Model.ReactiveModel.Events

-- Implement this interface if you want automatic update notification
-- import Hails.MVC.Model.ProtectedModel.UpdatableModel

data ModelEvent = UncapturedEvent
                | Initialised
--                | MaxVersionAvailable
 deriving (Eq,Ord)

instance GRM.Event ModelEvent where
  undoStackChangedEvent = UncapturedEvent

-- instance UpdateNotifiableEvent ModelEvent where
--   updateNotificationEvent = MaxVersionAvailable
 
instance InitialisedEvent ModelEvent where
  initialisedEvent = Initialised


