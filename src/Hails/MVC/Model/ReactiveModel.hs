{-# LANGUAGE ExistentialQuantification #-} 
{-# LANGUAGE DeriveDataTypeable #-}
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
module Hails.MVC.Model.ReactiveModel
   ( ReactiveModel (basicModel)
   -- * Construction
   , Event(..)
   , emptyRM
   -- * Access
   , pendingEvents
   , pendingHandlers
   , nextModels
   , previousModels
   -- * Modification
   , onBasicModel
   , onEvent
   , getPendingHandler
   , triggerEvent
   , triggerEvents

   -- * Handling the Undo/Redo stack
   , recordChange
   , onUndo
   , undo
   , redo
   , clearUndoStack
   , onUndoStack
     
   -- , FullEvent(..)
   -- , UndoEvent(..)
   )
  where

-- External imports
import           Control.Arrow    (first)
import           Control.Monad    (join)
import qualified Data.Foldable    as F
import           Data.Maybe       (fromJust)
import qualified Data.Map         as M
import           Data.Sequence    ((<|),(|>), (><), Seq, ViewL(..), ViewR(..), viewl, viewr)
import qualified Data.Sequence    as Seq
import           Data.Stack       as Stk
import           Data.Traversable as T
import           Data.Typeable    

-- | A reactive model uses an event datatype with all the events that our model
-- must trigger. An heterogenous container cannot be used because we need an Eq
-- operation that is efficient (a string comparison is not).
--
-- Therefore, we can declare operations that require certain events,
-- as long as we create a typeclass for Event types that have a constructor
-- for the kind of events we require. This reactive model handles Undo/Redo
-- internally, and changes to the undo-stack are notified automatically.
-- All Event types must declare an undo event, even if it's not used.
--
-- NOTE: This is experimental code. Undo/Redo support may not be necessary in
-- many programs, and another Reactive Model definition could be provided with
-- no support for undo-redo if this bothers you too much.
--
class (Eq a, Ord a) => Event a where
   undoStackChangedEvent :: a

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

-- data UndoEvent = UndoEvent
--  deriving (Eq, Ord, Typeable, Show)
          
-- instance Event UndoEvent where          
          
-- | A model of kind a with a stack of events of kind b
data Event b => ReactiveModel a b c = ReactiveModel 
  { basicModel      :: a
  , previousModels  :: Stack (a, Seq b )
  , nextModels      :: Stack (a, Seq b )
  , eventHandlers   :: M.Map b (Seq c)
  , pendingEvents   :: Seq b
  , pendingHandlers :: Seq c
  }

-- | Default constructor (with an empty model, no events and no handlers installed)
emptyRM :: Event b => a -> ReactiveModel a b c
emptyRM emptyBM = ReactiveModel
  { basicModel      = emptyBM
  , previousModels  = Stk.empty
  , nextModels      = Stk.empty
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
       hs2 = join $ T.mapM (\e -> M.findWithDefault Seq.empty e m) evs

-- | Record a change in the undo stack, with a list of associated events for
-- that change. Events are expected to work bi-directionally (the same event
-- will be triggered when the change is redone or undone).
recordChange :: Event b => ReactiveModel a b c -> (a -> a) -> [b] -> ReactiveModel a b c
recordChange rm f evs = triggerEvent rm' undoStackChangedEvent
  where rm' = rm { basicModel     = f (basicModel rm)
                 , previousModels = (basicModel rm, Seq.fromList evs) : previousModels rm
                 , nextModels     = Stk.empty
                 }

-- | Install a handler in the previous model's event list
onUndo :: Event b => ReactiveModel a b c -> [b] -> ReactiveModel a b c
onUndo rm evs =
  case pvs of
   ((bx, evx):xs) -> rm { previousModels = (bx, evx >< Seq.fromList evs):xs }
   _              -> rm
 where pvs = previousModels rm

-- | Undo one step
undo :: Event b => ReactiveModel a b c -> ReactiveModel a b c
undo rm = undo' rm (previousModels rm)

undo' :: Event b => ReactiveModel a b c -> Stack (a, Seq b) -> ReactiveModel a b c
undo' rm stk
 | null stk  = rm
 | otherwise = triggerEvents rm' (evx |> undoStackChangedEvent)
 where ((bx,evx),xs) = pop stk
       rm' = rm { basicModel     = bx
                , previousModels = xs
                , nextModels     = push (basicModel rm, evx) (nextModels rm)
                }

-- | Redo one step
redo :: Event b => ReactiveModel a b c -> ReactiveModel a b c
redo rm = redo' rm (nextModels rm)

redo' :: Event b => ReactiveModel a b c -> Stack (a , Seq b) -> ReactiveModel a b c
redo' rm stk 
  | null stk  = rm
  | otherwise = triggerEvents rm' (evx |> undoStackChangedEvent)
 where ((bx, evx),xs) = pop stk
       rm' = rm { basicModel     = bx
                , previousModels = push (basicModel rm, evx) (previousModels rm)
                , nextModels     = xs
                }

-- | Clear the undo stack (remove all known previous and next models)
clearUndoStack :: Event b => ReactiveModel a b c -> ReactiveModel a b c
clearUndoStack rm = 
  case (previousModels rm, nextModels rm) of
   ([],[]) -> rm
   _       -> let rm' = rm { previousModels = Stk.empty
                           , nextModels = Stk.empty 
                           }
              in triggerEvent rm' undoStackChangedEvent

-- | Apply a change to all the models in the undo stack
onUndoStack :: Event b => ReactiveModel a b c -> (a -> a) -> ReactiveModel a b c
onUndoStack rm f = rm { previousModels = map (first f) $ previousModels rm
                      , nextModels     = map (first f) $ nextModels rm
                      }
