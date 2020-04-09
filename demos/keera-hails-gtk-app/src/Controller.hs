-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013-2020
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
--
-- This contains the main controller. Many operations will be
-- implemented in the Controller.* subsystem. This module simply
-- initialises program.

-- FIXME: A debug version could be included as a separate controller.

module Controller where

-- Uncomment the following line if you need to capture errors
-- import System.Glib.GError

import Hails.MVC.View (Null (Null), createView, initView, startView)

-- Internal imports
import CombinedEnvironment
import Controller.Conditions
import Model.Model

-- If you have a main window to show, uncomment the following:
import Graphics.UI.Gtk (widgetShowAll)
import View.Objects (mainWindow)

-- | Starts the program by creating the model,
-- the view, starting all the concurrent threads,
-- installing the handlers for all the conditions
-- and starting the view.
startController :: IO ()
startController = do
  -- Uncomment the following line if you need to debug errors
  -- handleGError (\(GError _ _ em) -> putStrLn em) $ do

    -- Initialise the visual layer
    initView (Null :: Null (GtkView View))

    v <- createView :: IO (GtkView View)

    -- Create an empty model
    cenv <- createCEnv emptyBM

    -- Install the model and view handlers
    installHandlers cenv

    -- Notify the system's initialisation
    initialiseSystem $ model cenv

    -- If you have a main window, show it with the following
    -- instruction.
    mainWindow (uiBuilder (view cenv)) >>= widgetShowAll

    -- Run the view
    startView v
