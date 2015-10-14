-- | Publishes the main elements of an entry as reactive fields
module Graphics.UI.Gtk.Reactive.Entry where

import Graphics.UI.Gtk
import Data.ReactiveValue

entryTextReactive :: Entry -> ReactiveFieldReadWrite String
entryTextReactive e = ReactiveFieldReadWrite setter getter notifier
 where getter     = get e entryText
       setter v   = set e [entryText := v]
       notifier p = do on e entryPasteClipboard p
                       on e entryCutClipboard p
                       on e entryDeleteFromCursor (\_ _ -> p)
                       on e entryInsertAtCursor (\_ -> p)
                       on e entryPreeditChanged (\_ -> p)
                       return ()
