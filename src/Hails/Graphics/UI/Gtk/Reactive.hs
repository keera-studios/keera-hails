-- | Contains functions to define fields from Gtk widgets. In reality,
-- most widgets have different properties, all of which can be treated
-- as different fields. This module oversimplifies that vision and
-- provides one field for the most "important" of those attributes: a string
-- field for a text-box, a bool field for a checkbox, etc.
-- 
-- This module is deeply incomplete. Feel free to add other definitions
-- in you find them useful.
--
-- Also, it should not be here at all. Instead, it should be moved
-- to a separate library.
module Hails.Graphics.UI.Gtk.Reactive
   ( module Hails.Graphics.UI.Gtk.Reactive
   , module Exported
   )
  where

-- External libraries
import Control.Monad
import Graphics.UI.Gtk

-- Internal libraries
import Hails.Graphics.UI.Gtk.Helpers.Combo
import Hails.MVC.Controller.Reactive as Exported

-- An Entry's main reactive view field is a string with the contents
-- of the text box
type ReactiveViewEntry = ReactiveViewField String

reactiveEntry :: Entry -> ReactiveViewEntry
reactiveEntry entry = ReactiveViewField
 { onChange = void . (onEditableChanged entry)
 , rvfGet   = get entry entryText
 , rvfSet   = \t -> set entry [ entryText := t ]
 }

reactiveCheckMenuItem :: CheckMenuItem -> ReactiveViewField Bool
reactiveCheckMenuItem item = ReactiveViewField
 { onChange = void . (on item checkMenuItemToggled)
 , rvfGet   = checkMenuItemGetActive item
 , rvfSet   = checkMenuItemSetActive item
 }

reactiveToggleButton :: ToggleButtonClass a => a -> ReactiveViewField Bool
reactiveToggleButton item = ReactiveViewField
 { onChange = void . (on item toggled)
 , rvfGet   = toggleButtonGetActive item
 , rvfSet   = toggleButtonSetActive item
 }

reactiveSpinButton :: SpinButtonClass a => a -> ReactiveViewField Int
reactiveSpinButton item = ReactiveViewField
 { onChange = void . (onValueSpinned item)
 , rvfGet   = spinButtonGetValueAsInt item
 , rvfSet   = spinButtonSetValue item . fromIntegral
 }

reactiveTypedComboBoxUnsafe :: (Eq a) => ListStore a -> ComboBox -> ReactiveViewField a
reactiveTypedComboBoxUnsafe ls item = ReactiveViewField
 { onChange = void . (on item changed)
 , rvfGet   = typedComboBoxGetSelectedUnsafe (item, ls)
 , rvfSet   = typedComboBoxSetSelected (item, ls)
 }