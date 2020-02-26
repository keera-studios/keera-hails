-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
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

widgetSensitiveReactive :: WidgetClass self => self -> ReactiveFieldReadWrite IO Bool
widgetSensitiveReactive e = ReactiveFieldReadWrite setter getter notifier
 where getter     = get e widgetSensitive
       setter v   = postGUIAsync $ do
                      p <- getter
                      when (p /= v) $ set e [ widgetSensitive := v ]
       notifier p = void (e `on` mapEvent $ liftIO p >> return False)
