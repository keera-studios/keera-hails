{-# LANGUAGE MultiParamTypeClasses #-}
-- | Publishes the main elements of a status icon
module Graphics.UI.Gtk.Reactive.StatusIcon where

import Control.Monad
import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk
import Data.ReactiveValue

statusIconActivateField :: StatusIcon -> ReactiveFieldActivatable IO
statusIconActivateField m = mkActivatable op
 where op f = void (m `on` statusIconActivate $ liftIO f)

instance ReactiveValueActivatable IO StatusIcon where
  defaultActivation = statusIconActivateField
