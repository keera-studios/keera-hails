-- | Publishes the main elements of an entry as reactive fields
module Graphics.UI.Gtk.Reactive.Entry where

import Control.Monad (void)
import Graphics.UI.Gtk
import Data.ReactiveValue

entryTextReactive :: (EditableClass e, EntryClass e) => e -> ReactiveFieldReadWrite String
entryTextReactive e = ReactiveFieldReadWrite setter getter notifier
 where getter     = get e entryText
       setter v   = set e [entryText := v]
       notifier p = void (on e editableChanged p)
