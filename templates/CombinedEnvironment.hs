-- | The environment that contains both the view and the model.
--
-- | FIXME: In a very rails-like move, this module will probably be
-- exactly the same for all programs, so we should try to put
-- a "Convention-over-configuration" policy in place and remove this
-- unless it must be adapted by the user.
module CombinedEnvironment
   ( CEnv
   , module Exported
   )
  where

-- Generic libraries
import qualified Hails.MVC.GenericCombinedEnvironment as GEnv
import Hails.MVC.DefaultGtkEnvironment                as Exported

-- Internal libraries
import Model.ReactiveModel.ModelEvents                as Exported
import Model.ProtectedModel                           as Exported
import Model.Model  
import View                                           as Exported
import View.MainWindow.Objects                        as Exported

-- The simplest definition: a view, a model, and a set of events
type CEnv = GEnv.CEnv View Model ModelEvent