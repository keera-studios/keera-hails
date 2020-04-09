-- | A combined environment combines a view and a model in a MVC-structured
--   application. The environment can be passed around to the controller, which
--   must update each side accordingly and keep both in sync.
--
--   In this package, the view is built using GTK, and the model is implemented
--   as a 'ProtectedModel' (a safe-thread structure with a fine-grained notion
--   of change).
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.MVC.GenericCombinedEnvironment
    ( -- * Combined Environments (CEnvs)
      CEnv (view, model)
      -- * CEnv construction
    , createCEnv
      -- * CEnv updates
    , installCondition
    , installConditions
    )
  where

-- Internal libraries
import Hails.MVC.Model.ProtectedModel (ProtectedModel, startProtectedModel)
import Hails.MVC.Model.ReactiveModel  (Event)
import Hails.MVC.View                 (createView)
import Hails.MVC.View.GtkView         (GtkGUI, GtkView)

-- | Given a GUI and a type for the events that capture the changes inside the
--   model, a CEnv contains a view and a protected model.
data (GtkGUI a, Event c) => CEnv a b c = CEnv
  { view  :: GtkView a          -- ^ View in the CEnv of an MVC application.
  , model :: ProtectedModel b c -- ^ Model in the CEnv of an MVC application.
  }

-- | Create a combined environment from a given model and initialize the
--   model and the view. The default model must be provided explicitly.
--   The initialisation operations for the view and the protected model
--   are called internally.
createCEnv :: (GtkGUI a, Event c) => b -> IO (CEnv a b c)
createCEnv emptyBM = CEnv <$> createView
                          <*> startProtectedModel emptyBM

-- | Install a condition in a Combined Environment. Conditions, or
--   reactive rules, are used to keep both the model and the view
--   in sync.

-- NOTE: This is an experimental function and might be removed in the future.
installCondition :: (GtkGUI a, Event c)
                 => CEnv a b c
                 -> (CEnv a b c -> IO ())
                 -> IO ()
installCondition cenv cond = cond cenv

-- | Install several conditions in a Combined Environment. Conditions, or
--   reactive rules, are used to keep both the model and the view
--   in sync.

-- FIXME: I really don't like the syntax
--   installConditions cenv
--      [ rv1 =:= rf1
--      , ...
--      ]
--
--   I'd rather use
--   installConditions cenv $ do
--     rv1 =:= rf1
--     rv2 =:= rf2
--     ...
--   Which means that I would have to define a monad.
--
-- NOTE: This is an experimental function and might be removed in the future.
installConditions :: (GtkGUI a, Event c)
                  => CEnv a b c
                  -> [CEnv a b c -> IO ()]
                  -> IO ()
installConditions cenv conds = mapM_ (installCondition cenv) conds
