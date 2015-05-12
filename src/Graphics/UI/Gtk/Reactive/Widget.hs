module Graphics.UI.Gtk.Reactive.Widget where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, when)
import Data.ReactiveValue
import Graphics.UI.Gtk

widgetVisibleReactive :: WidgetClass self => self -> ReactiveFieldReadWrite IO Bool
widgetVisibleReactive e = ReactiveFieldReadWrite setter getter notifier
 where getter     = get e widgetVisible
       setter v   = postGUIAsync $ do
                      p <- getter
                      when (p /= v) $ set e [ widgetVisible := v ]
       notifier p = void (e `on` mapEvent $ liftIO p >> return False)
