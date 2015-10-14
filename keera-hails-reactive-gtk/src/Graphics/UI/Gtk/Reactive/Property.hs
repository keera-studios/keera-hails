{-# LANGUAGE ScopedTypeVariables #-}
-- | Publishes the main elements of a scale as reactive fields
module Graphics.UI.Gtk.Reactive.Property where

import Control.Monad (void, when)
import Control.Monad.IO.Class
import Graphics.UI.Gtk
import Data.ReactiveValue

-- * Attributes as reactive values

-- ** Signal-based reactimation functions.

-- | Create an RV based on a widget's attribute and signal. Before setting,
-- the value is checked against the current one. If they are the same, the
-- value is *not* set.
reactiveProperty :: Eq b
                 => self
                 -> Signal self (IO ()) -> Attr self b
                 -> ReactiveFieldReadWrite IO b
reactiveProperty e sig attr =
  ReactiveFieldReadWrite setter getter notifier
 where getter     = get e attr
       setter v   = postGUIAsync $ do
                      p <- getter
                      when (p /= v) $ set e [ attr := v ]
       notifier p = void (on e sig p)

-- | Create an RV based on a widget's attribute and signal.
-- Before setting, the value is *not* checked against the current one.
-- The value is thus set even if they are the same. 
reactivePropertyNE :: self
                   -> Signal self (IO ()) -> Attr self b
                   -> ReactiveFieldReadWrite IO b
reactivePropertyNE e sig attr =
  ReactiveFieldReadWrite setter getter notifier
 where getter     = get e attr
       setter v   = postGUIAsync $ set e [ attr := v ]
       notifier p = void (on e sig p)

reactiveSignalIO :: self
                 -> Signal self (IO ())
                 -> ReactiveFieldRead IO ()
reactiveSignalIO e sig =
  ReactiveFieldRead getter notifier
 where getter     = return ()
       notifier p = void (on e sig p)

reactiveSignalM :: (Monad m, MonadIO m)
                => self
                -> a
                -> Signal self (m a)
                -> ReactiveFieldRead IO ()
reactiveSignalM e def sig =
  ReactiveFieldRead getter notifier
 where getter     = return ()
       notifier p = void (on e sig (liftIO p >> return def))


-- ** Handler-based reactimation functions

-- | Create an RV based on a widget's attribute and a handler.
-- Before setting, the value is checked against the current one.
-- If they are the same, the value is *not* set.
reactivePropertyH :: Eq b
                  => self
                  -> (self -> IO () -> IO (ConnectId self)) -> Attr self b
                  -> ReactiveFieldReadWrite IO b
reactivePropertyH e sig attr =
  ReactiveFieldReadWrite setter getter notifier
 where getter     = get e attr
       setter v   = postGUIAsync $ do
                      p <- getter
                      when (p /= v) $ set e [ attr := v ]
       notifier p = void (e `sig` p)

-- | Create a unit read-only RV based on a widget's event handler.
reactivePropertyH_ :: self
                   -> (self -> IO () -> IO (ConnectId self))
                   -> ReactiveFieldRead IO ()
reactivePropertyH_ e sig =
  ReactiveFieldRead getter notifier
 where getter     = return ()
       notifier p = void (e `sig` p)

-- * Passive properties

-- | A passive reactive value is one that does not report when it changes.
--
-- This function returns a RW RV that encloses the given property, without
-- firing change events. The value of the attribute is *not* set if it is the
-- same as the current one.
--
-- To set without diffing, see 'passivePropertyNE'.
passiveProperty :: Eq b
                 => self
                 -> Attr self b
                 -> ReactiveFieldReadWrite IO b
passiveProperty e attr =
  ReactiveFieldReadWrite setter getter notifier
 where getter     = get e attr
       setter v   = postGUIAsync $ do
                      p <- getter
                      when (p /= v) $ set e [ attr := v ]
       notifier _ = return ()

-- | Return a RW RV that encloses the given property, without firing change
-- events.
--
-- When writing to this RV, the value is *not* diffed against the previous one.
-- The underlying widget can thus still fire signals based on that change.
passivePropertyNE :: self
                  -> Attr self b
                  -> ReactiveFieldReadWrite IO b
passivePropertyNE e attr =
  ReactiveFieldReadWrite setter getter notifier
 where getter     = get e attr
       setter v   = postGUIAsync $ set e [ attr := v ]
       notifier _ = return ()

passivePropertyGE :: Eq b
                 => self
                 -> (self -> b -> IO ()) -> (self -> IO b)
                 -> ReactiveFieldReadWrite IO b
passivePropertyGE e set get =
  ReactiveFieldReadWrite setter getter notifier
 where getter     = get e
       setter v   = postGUIAsync $ do
                      p <- getter
                      when (p /= v) $ set e v
       notifier _ = return ()
