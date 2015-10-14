{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Hails.Graphics.UI.Gtk.Simplify.ReactiveGtk where

import Control.Monad (void)
import Control.Monad.Reader (liftIO)

import Graphics.UI.Gtk

import Data.ReactiveValue

menuItemActivateField :: MenuItem -> ReactiveFieldActivatable 
menuItemActivateField m = mkActivatable op
 where op f = void (m `on` menuItemActivate $ liftIO f)

buttonActivateField :: Button -> ReactiveFieldActivatable 
buttonActivateField b = mkActivatable op
 where op f = void (b `onClicked` f)

toolButtonActivateField :: ToolButton -> ReactiveFieldActivatable 
toolButtonActivateField t = mkActivatable op
 where op f = void (t `onToolButtonClicked` f)

instance ReactiveValueActivatable ToolButton where
  defaultActivation = toolButtonActivateField

instance ReactiveValueActivatable Button where
  defaultActivation = buttonActivateField

instance ReactiveValueActivatable MenuItem where
  defaultActivation = menuItemActivateField
