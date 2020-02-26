{-# LANGUAGE MultiParamTypeClasses #-}
-- | Publishes the main elements of a menuitem
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Graphics.UI.Gtk.Reactive.Button where

import Control.Monad
import Graphics.UI.Gtk
import Data.ReactiveValue

buttonActivateField :: Button -> ReactiveFieldActivatable IO
buttonActivateField b = mkActivatable op
 where op f = void (b `onClicked` f)

instance ReactiveValueActivatable IO Button where
  defaultActivation = buttonActivateField
