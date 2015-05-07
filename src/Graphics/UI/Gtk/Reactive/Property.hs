-- | Publishes the main elements of a scale as reactive fields
module Graphics.UI.Gtk.Reactive.Property where

import Control.Monad (void)
import Graphics.UI.Gtk
import Data.ReactiveValue

reactiveProperty :: self -> Signal self (IO ()) -> Attr self b
                 -> ReactiveFieldReadWrite IO b
reactiveProperty e sig attr = ReactiveFieldReadWrite setter getter notifier
 where getter     = get e attr
       setter v   = set e [ attr := v ]
       notifier p = void (on e sig p)
