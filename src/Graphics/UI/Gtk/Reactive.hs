-- | Contains functions to define fields from Gtk widgets. In reality,
-- most widgets have different properties, all of which can be treated
-- as different fields. This module oversimplifies that vision and
-- provides one field for the most "important" of those attributes: a
-- string field for a text-box, a bool field for a checkbox, etc.
-- 
-- This module is deeply incomplete. Feel free to add other definitions
-- in you find them useful.
--
-- Also, it should not be here at all. Instead, it should be moved
-- to a separate library.
module Graphics.UI.Gtk.Reactive
   ( module Graphics.UI.Gtk.Reactive
   , module Exported
   )
  where

-- External libraries
import Graphics.UI.Gtk

-- Internal libraries
import qualified Graphics.UI.Gtk.GtkView              as GtkView
import Hails.Graphics.UI.Gtk.Reactive
import Hails.MVC.Model.ReactiveModel
import Hails.MVC.View.Reactive
import Hails.MVC.View.GladeView
import Hails.MVC.Controller.Reactive                  as Exported
import qualified Hails.MVC.GenericCombinedEnvironment as GEnv
import Hails.MVC.DefaultGtkEnvironment

type GtkCEnv a b c = GEnv.CEnv a b c

-- | Accesses a Reactive View Field from a CEnv
cenvReactiveField :: (GtkView.GtkGUI c, GladeView c, Event e)
                     => (a -> ReactiveViewField b) -> Accessor a
                     -> GtkCEnv c d e -> IO (ReactiveViewField b)
cenvReactiveField f entryF cenv = 
  fmap f $ entryF $ ui $ view cenv

-- A Reactive View Entry is just a String reactive field
cenvReactiveEntry :: (GtkView.GtkGUI a, GladeView a, Event c)
                     => Accessor Entry -> GtkCEnv a b c
                     -> IO ReactiveViewEntry
cenvReactiveEntry = cenvReactiveField reactiveEntry

-- A Menu Item's reactive view is a boolean that represents whether
-- the item is active or not
cenvReactiveCheckMenuItem :: (GtkView.GtkGUI a, GladeView a, Event c)
                             => Accessor CheckMenuItem -> GtkCEnv a b c
                             -> IO (ReactiveViewField Bool)
cenvReactiveCheckMenuItem = cenvReactiveField reactiveCheckMenuItem

-- A Toggle Button's reactive view is a boolean that represents whether
-- the item is active or not
cenvReactiveToggleButton :: (ToggleButtonClass a,
                             GtkView.GtkGUI b, GladeView b, Event d)
                            => Accessor a -> GtkCEnv b c d
                            -> IO (ReactiveViewField Bool)
cenvReactiveToggleButton = cenvReactiveField reactiveToggleButton

-- A spin button's reactive view is an integer with the value represented
-- by the text it holds
cenvReactiveSpinButton :: (SpinButtonClass a, 
                           GtkView.GtkGUI b, GladeView b, Event d)
                          => Accessor a -> GtkCEnv b c d
                          -> IO (ReactiveViewField Int)
cenvReactiveSpinButton = cenvReactiveField reactiveSpinButton

-- A Combo box's reactive view is the selected element.
-- Because there's no guarantee that the liststore corresponds
-- to the given combo box, this function is unsafe.
cenvReactiveTypedComboBoxUnsafe :: (Eq a, GtkView.GtkGUI b, GladeView b
                                   , Event d) 
                                   => ListStore a -> Accessor ComboBox
                                   -> GtkCEnv b c d
                                   -> IO (ReactiveViewField a)
cenvReactiveTypedComboBoxUnsafe = 
  cenvReactiveField . reactiveTypedComboBoxUnsafe
