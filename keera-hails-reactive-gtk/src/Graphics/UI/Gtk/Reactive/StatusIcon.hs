{-# LANGUAGE MultiParamTypeClasses #-}
-- | Publishes the main elements of a status icon
module Graphics.UI.Gtk.Reactive.StatusIcon where

import Control.Monad
import Control.Monad.Trans(liftIO)
import Data.ReactiveValue
import Graphics.UI.Gtk

statusIconActivateField :: StatusIcon -> ReactiveFieldActivatable IO
statusIconActivateField m = mkActivatable op
 where op f = void (m `on` statusIconActivate $ liftIO f)

instance ReactiveValueActivatable IO StatusIcon where
  defaultActivation = statusIconActivateField

statusIconVisibleReactive :: StatusIcon -> ReactiveFieldReadWrite IO Bool
statusIconVisibleReactive icon = ReactiveFieldReadWrite setter getter notifier
 where getter     = statusIconGetVisible icon
       setter v   = postGUIAsync $ do
                      p <- getter
                      when (p /= v) $ statusIconSetVisible icon v
       notifier _ = return ()
