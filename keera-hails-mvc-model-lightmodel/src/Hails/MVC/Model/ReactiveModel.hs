{-# LANGUAGE ExistentialQuantification #-}
-- | This module holds a reactive program model. It holds a program model, but
-- includes events that other threads can listen to, so that a change in a part
-- of the model is notified to another part of the program. The reactive model
-- is not necessarily concurrent (it doesn't have its own thread), although a
-- facility is included to make it also concurrent (so that event handlers can
-- be called as soon as they are present).
--
-- This type includes operations to handle undoing-redoing and
-- tracking which notifications must be triggered in each
-- undo-redo step.
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.MVC.Model.ReactiveModel
   ( ReactiveModel (basicModel)
   -- * Construction
   , Event
   , emptyRM
   -- * Access
   , pendingEvents
   , pendingHandlers
   -- * Modification
   , onBasicModel
   , onEvent
   , onEvents
   , getPendingHandler
   , eventHandlers
   , prepareEventHandlers
   , triggerEvent
   , triggerEvents

   )
  where

-- External imports
import qualified Data.Foldable    as F
import qualified Data.Map         as M
import           Data.Sequence    ((|>), (><), Seq, ViewL(..), viewl)
import qualified Data.Sequence    as Seq

-- | A reactive model uses an event datatype with all the events that our model
-- must trigger. An heterogenous container cannot be used because we need an Eq
-- operation that is efficient (a string comparison is not).
--
-- Therefore, we can declare operations that require certain events,
-- as long as we create a typeclass for Event types that have a constructor
-- for the kind of events we require.
--
-- NOTE: This is experimental code.
--
class (Eq a, Ord a) => Event a where

-- data FullEvent = forall a . Event a => FullEvent a

-- instance Eq FullEvent where
--   (FullEvent a) == (FullEvent b) = typeOf a == typeOf b
--                                    && cast a == Just b
-- instance Ord FullEvent where
--   (FullEvent a) < (FullEvent b) = (typeOf a == typeOf b
--                                    && fromJust (cast a) < b)
--                                   || (show (typeOf a) < show (typeOf b))

-- instance Show FullEvent where
--   show (FullEvent x) = show x

-- | A model of kind a with a stack of events of kind b
data Event b => ReactiveModel a b c = ReactiveModel
  { basicModel      :: a
  , eventHandlers   :: M.Map b (Seq c)
  , pendingEvents   :: Seq b
  , pendingHandlers :: Seq c
  }

-- | Default constructor (with an empty model, no events and no handlers installed)
emptyRM :: Event b => a -> ReactiveModel a b c
emptyRM emptyBM = ReactiveModel
  { basicModel      = emptyBM
  , eventHandlers   = M.empty
  , pendingEvents   = Seq.empty
  , pendingHandlers = Seq.empty
  }

-- | Apply a modification to the internal model (no events are triggered)
onBasicModel :: Event b => ReactiveModel a b c -> (a -> a) -> ReactiveModel a b c
onBasicModel rm f = rm { basicModel = f (basicModel rm) }

-- | Install a handler for an event
onEvent :: Event b => ReactiveModel a b c -> b -> c -> ReactiveModel a b c
onEvent rm ev f = rm { eventHandlers = m' }
 where ls  = M.findWithDefault Seq.empty ev m
       ls' = ls |> f
       m   = eventHandlers rm
       m'  = M.insert ev ls' m

onEvents :: (F.Foldable container, Event b) => ReactiveModel a b c -> container b -> c -> ReactiveModel a b c
onEvents rm evs f = F.foldl (\rm' e' -> onEvent rm' e' f) rm evs

-- | Trigger an event (execute all handlers associated to it)
triggerEvent :: Event b => ReactiveModel a b c -> b -> ReactiveModel a b c
triggerEvent rm e = rm { pendingEvents = ps' }
  where ps  = pendingEvents rm
        ps' = ps |> e

-- | Trigger many events in sequence (execute all handlers associated to them)
triggerEvents :: Event b => ReactiveModel a b c -> Seq b -> ReactiveModel a b c
triggerEvents = F.foldl triggerEvent

-- | If any pending handler exists or can be obtained, it is returned
-- and removed from the queue
getPendingHandler :: Event b => ReactiveModel a b c -> (ReactiveModel a b c, Maybe c)
getPendingHandler rm = (rm' { pendingHandlers = pt }, ph)
 where rm'      = prepareEventHandlers rm
       ps       = pendingHandlers rm'
       vw       = viewl ps
       (ph, pt) = case vw of
                    EmptyL    -> (Nothing, ps)
                    (h :< hs) -> (Just h, hs)
                  -- if Seq.null ps then (Nothing,ps) else (Just (head ps), tail ps)

-- | Return a reactive model that has no pending events. All the pending events
-- have been looked up in the eventHandlers table and the handlers have been
-- added to the field pendingHandlers.
prepareEventHandlers :: Event b => ReactiveModel a b c -> ReactiveModel a b c
prepareEventHandlers rm =
  rm { pendingEvents = Seq.empty, pendingHandlers = hs1 >< hs2 }
 where evs = pendingEvents rm
       m   = eventHandlers rm
       hs1 = pendingHandlers rm
       hs2 = F.foldl (><) Seq.empty $
                  fmap (\e -> M.findWithDefault Seq.empty e m) evs
