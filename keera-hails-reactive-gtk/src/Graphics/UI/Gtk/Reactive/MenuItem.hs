{-# LANGUAGE MultiParamTypeClasses #-}
-- | Publishes the main elements of a menuitem
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Graphics.UI.Gtk.Reactive.MenuItem where

import Control.Monad
import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk
import Data.ReactiveValue

menuItemActivateField :: MenuItem -> ReactiveFieldActivatable IO
menuItemActivateField m = mkActivatable op
 where op f = void (m `on` menuItemActivate $ liftIO f)

instance ReactiveValueActivatable IO MenuItem where
  defaultActivation = menuItemActivateField
