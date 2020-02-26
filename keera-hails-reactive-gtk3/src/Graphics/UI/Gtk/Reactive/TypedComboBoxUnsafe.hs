-- | Publishes the main elements of a toggle button
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Graphics.UI.Gtk.Reactive.TypedComboBoxUnsafe where

import Control.Monad
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Helpers.Combo
import Data.ReactiveValue

typedComboBoxUnsafeReactive :: (Eq a) => ListStore a -> ComboBox -> ReactiveFieldReadWrite IO a
typedComboBoxUnsafeReactive ls e = ReactiveFieldReadWrite setter getter notifier
 where getter   = typedComboBoxGetSelectedUnsafe (e, ls)
       setter   = typedComboBoxSetSelected (e, ls)
       notifier = void . (on e changed)
