{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Publishes the main elements of an entry as reactive fields
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Graphics.UI.Gtk.Reactive.Entry where

import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive.Property

entryTextReactive :: (EditableClass e, EntryClass e) => e -> ReactiveFieldReadWrite IO String
entryTextReactive e = reactiveProperty e editableChanged entryText

-- import Control.Monad (void, when)
-- entryTextReactive :: (EditableClass e, EntryClass e) => e -> ReactiveFieldReadWrite IO String
-- entryTextReactive e = ReactiveFieldReadWrite setter getter notifier
--  where getter     = get e entryText
--        setter v   = postGUIAsync $ do
--                       p <- get e entryText
--                       when (p /= v) $ set e [entryText := v]
--        notifier p = void (on e editableChanged p)

instance ReactiveValueReadWrite Entry String IO where

instance ReactiveValueRead Entry String IO where
 reactiveValueOnCanRead = reactiveValueOnCanRead . entryTextReactive
 reactiveValueRead      = reactiveValueRead . entryTextReactive

instance ReactiveValueWrite Entry String IO where
 reactiveValueWrite = reactiveValueWrite . entryTextReactive
