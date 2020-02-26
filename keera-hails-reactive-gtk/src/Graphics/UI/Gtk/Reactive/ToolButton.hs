{-# LANGUAGE MultiParamTypeClasses #-}
-- | Publishes the main elements of a menuitem
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Graphics.UI.Gtk.Reactive.ToolButton where

import Control.Monad
import Graphics.UI.Gtk
import Data.ReactiveValue

toolButtonActivateField :: ToolButton -> ReactiveFieldActivatable IO
toolButtonActivateField b = mkActivatable op
 where op f = void (b `onToolButtonClicked` f)

instance ReactiveValueActivatable IO ToolButton where
  defaultActivation = toolButtonActivateField
