{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Hails.Graphics.UI.Gtk.Simplify.ReactiveGtk where

import Control.Monad.Reader (liftIO)

import Graphics.UI.Gtk

import Data.ReactiveValue

instance ReactiveValueActivatable MenuItem where
  reactiveValueOnActivate mn op = do
    _  <- mn `on` menuItemActivate $ liftIO op
    return ()

instance ReactiveValueActivatable Button where
  reactiveValueOnActivate bt op = do
    _ <- bt `onClicked` op
    return ()

instance ReactiveValueActivatable ToolButton where
  reactiveValueOnActivate bt op = do
    _ <- bt `onToolButtonClicked` op
    return ()
