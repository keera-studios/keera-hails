module Graphics.UI.Gtk.Reactive.Window where

import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.ReactiveValue
import Graphics.UI.Gtk

windowCloseReactive :: WindowClass self => self -> ReactiveFieldRead IO ()
windowCloseReactive self = ReactiveFieldRead getter notifier
  where getter     = return ()
        notifier p = void (self `on` deleteEvent $ liftIO p >> return True)

windowVisibilityPassive :: WindowClass self => self -> ReactiveFieldReadWrite IO Bool
windowVisibilityPassive self = ReactiveFieldReadWrite setter getter (const $ return ())
 where setter x = do x' <- getter
                     when (x /= x') $
                       if x then widgetShowAll self else widgetHide self
       getter = get self widgetVisible
