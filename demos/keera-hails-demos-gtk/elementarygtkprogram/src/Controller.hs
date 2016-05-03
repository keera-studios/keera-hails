-- | This contains the main controller. Many operations will be
-- implemented in the Controller.* subsystem. This module simply
-- initialises program.
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
  cref <- createCRef emptyBM
  readIORef cref
    >>= mainWindow.mainWindowBuilder.view
    >>= widgetShowAll

  installHandlers cref

  startView
