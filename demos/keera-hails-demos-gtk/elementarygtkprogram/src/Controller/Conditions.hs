-- | This module contains a series of conditions that must hold between
-- the view and the model. Most of these conditions can be separated in
-- two conditions: one that must be checked only when the model changes
-- (and updates the view accordingly), and another that must be checked
-- when the view receives an event (and updates the model accordingly).
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Controller.Conditions
   ( installHandlers
   )
  where

-- External libraries
import Hails.MVC.View.GtkView

-- External libraries: general conditions
-- Close main window
import qualified Hails.Graphics.UI.Gtk.Simplify.ProgramMainWindow as PMW
-- Name the main window
import qualified Hails.Graphics.UI.Gtk.Simplify.NameAndVersionTitleBar as Title

-- Internal libraries
import CombinedEnvironment
import View
import View.MainWindow.Objects

-- Internal libraries: specific conditions

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  PMW.installHandlers cenv (mainWindow . mainWindowBuilder . getGUI)
  Title.installHandlers cenv (mainWindow . mainWindowBuilder . getGUI)
