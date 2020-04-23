-- | This contains the main controller. Many operations will be
-- implemented in the Controller.* subsystem. This module simply
-- initialises program.
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Controller where

-- External imports
import Data.IORef
import Graphics.UI.Gtk

-- Internal imports
import View (initView, startView, mainWindowBuilder)
import View.MainWindow.Objects
import CombinedEnvironment
import Controller.Conditions
import Model.Model

-- | Starts the program by creating the model,
-- the view, starting all the concurrent threads,
-- installing the hanlders for all the conditions
-- and starting the view.
startController :: IO ()
startController = do

  initView
  env <- createCEnv emptyBM
  w <- mainWindow $ mainWindowBuilder $ view env
  widgetShowAll w

  installHandlers env

  startView
