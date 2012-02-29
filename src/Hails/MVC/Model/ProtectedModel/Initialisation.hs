-- | Contains only one operation to notify that the system's been
-- initialised.
--
-- FIXME: In a very rails-like move, this module will likely be
-- exactly the same for all programs, so we should try to put
-- a "Convention-over-configuration" policy in place and remove this
-- unless it must be adapted by the user.
module Hails.MVC.Model.ProtectedModel.Initialisation where

import qualified Hails.MVC.Model.ReactiveModel.Initialisation as RM
import Hails.MVC.Model.ReactiveModel.Events
import Hails.MVC.Model.ReactiveModel
import Hails.MVC.Model.ProtectedModel

initialiseSystem :: InitialisedEvent c => ProtectedModel a c -> IO ()
initialiseSystem x = do
   putStrLn "System initialisation"
   x`applyToReactiveModel` RM.initialiseSystem
