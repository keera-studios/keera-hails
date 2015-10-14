module Hails.Graphics.UI.Gtk.Simplify.ProgramMainWindow where

import Control.Monad.Reader (liftIO)
import Hails.MVC.Model.ReactiveModel (Event)
import Hails.MVC.GenericCombinedEnvironment
import Graphics.UI.Gtk
import Hails.MVC.View
import Hails.MVC.View.GtkView

installHandlers :: (GtkGUI a, Event c)
                => CEnv a b c
                -> (ViewElementAccessorIO (GtkView a) Window)
                -> IO ()
installHandlers cenv wF = do
  let vw = view cenv
  w  <- wF vw
  _  <- w `on` deleteEvent $ liftIO $ condition cenv
  return ()

condition :: (GtkGUI a, Event c)
          => CEnv a b c
          -> IO Bool
condition cenv = do
  let vw = view cenv
  onViewAsync vw $ destroyView vw
  return False
