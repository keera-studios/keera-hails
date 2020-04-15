-- | Publishes the main elements of an entry as reactive fields
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Graphics.UI.Gtk.Reactive.Entry where

import Control.Monad (void)
import Graphics.UI.Gtk
import Data.ReactiveValue

entryTextReactive :: (EditableClass e, EntryClass e) => e -> ReactiveFieldReadWrite IO String
entryTextReactive e = ReactiveFieldReadWrite setter getter notifier
 where getter     = get e entryText
       setter v   = postGUIAsync $ set e [entryText := v]
       notifier p = void (on e editableChanged p)
