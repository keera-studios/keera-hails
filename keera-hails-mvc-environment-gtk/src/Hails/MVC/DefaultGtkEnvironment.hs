-- | Environment that contains both a model and a view.
--
--   Combined enviroments (CEnvs) are types that combine a model and
--   a view. They allow for easy installing of rules that keep both
--   in sync.
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.MVC.DefaultGtkEnvironment
    ( -- * Combined Environments (CEnvs)
      GEnv.CEnv
      -- * CEnv construction
    , GEnv.createCEnv
      -- * CEnv access
    , view
    , GEnv.model
      -- * CEnv updates
    , GEnv.installCondition
    , GEnv.installConditions
    )
  where

-- Internal libraries
import qualified Hails.MVC.GenericCombinedEnvironment as GEnv
import           Hails.MVC.Model.ReactiveModel        (Event)
import qualified Hails.MVC.View.GtkView               as GtkView

-- | Extract the GUI from a combined environement 'CEnv'.
view :: (GtkView.GtkGUI a, Event c) => GEnv.CEnv a b c -> a
view = GtkView.getGUI . GEnv.view
