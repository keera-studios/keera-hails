-- | Condition: The page of the main notebook visible at each moment
-- is the that corresponds to the selected branch in the category
-- tree.

module Hails.Graphics.UI.Gtk.Simplify.AboutDialog
    (installHandlers)
  where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Reader (liftIO)
import Data.ExtraVersion
import Graphics.UI.Gtk
import Hails.MVC.View.GtkView
import Hails.MVC.View
-- import Graphics.UI.Gtk.GenericView
import Hails.MVC.GenericCombinedEnvironment
import Hails.MVC.Model.ReactiveModel (Event)
import Hails.MVC.Model.ProtectedModel.VersionedModel
import Hails.MVC.Model.ProtectedModel.NamedModel

installHandlers :: (GtkGUI a, VersionedBasicModel b, NamedBasicModel b,
                    Event c, MenuItemClass d)
                => CEnv a b c
                -> ViewElementAccessorIO (GtkView a) d
                -> ViewElementAccessorIO (GtkView a) AboutDialog
                -> IO ()
installHandlers cenv mF dF = void $ do
  let vw = view cenv
  mn <- mF vw
  mn `on` menuItemActivate $ liftIO (onViewAsync vw (condition cenv dF))

condition :: (GtkGUI a, VersionedBasicModel b, NamedBasicModel b, Event c)
                => CEnv a b c
                -> ViewElementAccessorIO (GtkView a) AboutDialog
                -> IO ()
condition cenv dF = do
  let (vw, pm) = (view &&& model) cenv
  dg <- dF vw
  vn <- versionToString <$> getVersion pm
  pr <- getName pm
  set dg [ aboutDialogVersion := vn ]
  set dg [ aboutDialogProgramName := pr ]
  set dg [ aboutDialogName := pr ]
  _ <- dialogRun dg
  widgetHide dg
