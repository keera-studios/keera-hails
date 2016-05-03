-- | This module contains a series of conditions that must hold between
-- the view and the model. Most of these conditions can be separated in
-- two conditions: one that must be checked only when the model changes
-- (and updates the view accordingly), and another that must be checked
-- when the view receives an event (and updates the model accordingly).

module Controller.Conditions
   ( installHandlers
   )
  where

-- External libraries
import Graphics.UI.Gtk.GenericView

-- External libraries: general conditions
-- Close main window
import qualified Graphics.UI.Simplify.ProgramMainWindow as PMW
-- Name the main window
import qualified Graphics.UI.Simplify.NameAndVersionTitleBar as Title

-- Internal libraries
import CombinedEnvironment
import View
import View.MainWindow.Objects

-- Internal libraries: specific conditions

installHandlers :: CRef -> IO()
installHandlers cref = do
  PMW.installHandlers cref (mainWindow . mainWindowBuilder . getGUI)
  Title.installHandlers cref (mainWindow . mainWindowBuilder . getGUI)
