module Hails.Graphics.UI.Gtk.Simplify.NameAndVersionTitleBar where

import Control.Arrow
import Control.Monad
import Control.Monad.Reader (liftIO)
import Data.ExtraVersion
-- import Graphics.UI.Gtk.GenericView
import Graphics.UI.Gtk
import Graphics.UI.Gtk.GtkView
import Graphics.UI.View
import Hails.MVC.GenericCombinedEnvironment
import Hails.MVC.Model.ReactiveModel (Event)
import Hails.MVC.Model.ProtectedModel.VersionedModel
import Hails.MVC.Model.ProtectedModel.NamedModel

installHandlers :: (GtkGUI a, VersionedBasicModel b, NamedBasicModel b, Event c)
                => CEnv a b c
                -> (ViewElementAccessorIO (GtkView a) Window)
                -> IO ()
installHandlers cenv wF = void $ do
  let vw = view cenv
  w <- wF vw
  w `on` mapEvent $ liftIO (onViewAsync vw (condition cenv wF)) >> return False

condition :: (GtkGUI a, VersionedBasicModel b, NamedBasicModel b, Event c)
          => CEnv a b c
          -> (ViewElementAccessorIO (GtkView a) Window)
          -> IO ()
condition cenv wF = do
  let (vw, pm) = (view &&& model) cenv
  w  <- wF vw
  pn <- getName pm
  vn <- fmap versionToString $ getVersion pm
  t  <- windowGetTitle w
  let titleMust = pn ++ " " ++ vn
  when (t /= titleMust) $ windowSetTitle w titleMust
