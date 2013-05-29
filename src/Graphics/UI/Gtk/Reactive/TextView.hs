-- | Publishes the main elements of a text view as reactive fields
module Graphics.UI.Gtk.Reactive.TextView where

import Control.Monad (void)
import Graphics.UI.Gtk
import Data.ReactiveValue

textViewTextReactive :: TextView -> ReactiveFieldReadWrite String
textViewTextReactive e = ReactiveFieldReadWrite setter getter notifier
 where getter     = get e textViewBuffer >>= (`get` textBufferText)
       setter v   = get e textViewBuffer >>= (\b -> set b [textBufferText := v])
       notifier p = get e textViewBuffer >>= (\b -> void (on b bufferChanged p))
