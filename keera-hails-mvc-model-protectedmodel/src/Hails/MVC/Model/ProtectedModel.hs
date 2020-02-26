-- | Note: this is experimental code. It's what I'm using to build my
-- own Gtk apps. That being said, you may find IO more often than it's
-- really necessary. I'd be glad if you could point that out when you
-- see it. I'd like to make this code as generic and useful as
-- possible.
--
-- This module holds the protected reactive program model. It holds
-- a reactive model, but includes an interface that is thread safe
-- (can be called concurrently).  This makes it easier for different
-- threads to modify the model without having to worry about
-- concurrency. Note that using this interface can lead to deadlocks
-- in the program.
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.MVC.Model.ProtectedModel
   ( ProtectedModel (reactiveModel)
   -- * Construction
   , startProtectedModel
   -- * Access
   , onReactiveModel
   , onEvent
   , onEvents
   , applyToReactiveModel
   , fromReactiveModel
   , waitFor
   )
  where

-- External libraries
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe
import Data.Map      as M
import Data.Foldable as F
import Data.Sequence as Seq

-- Internal libraries
import Hails.MVC.Model.ReactiveModel
  ( emptyRM
  , getPendingHandler
  , pendingEvents
  , pendingHandlers
  , eventHandlers
  , prepareEventHandlers
  , Event
  , ReactiveModel
  )
import qualified Hails.MVC.Model.ReactiveModel as RM

-- A Protected model holds a reactive model and a thread that calls
-- the necessary event handlers as soon as the events are triggered.
-- Note that the hanlders are executed by this thread, which means
-- that, if you need the operation to be executed in another handlers,
-- you'll have to write explicit code for that.
--
-- Gtk (which is what I use this for) has specific functions for this
-- purpose.
data (Event b) => ProtectedModel a b = ProtectedModel
  { reactiveModel :: TVar (ReactiveModelIO a b)
  , dispatcher    :: Maybe ThreadId
  }

type ReactiveModelIO a b = ReactiveModel a b (IO ())

-- | Start executing the a new protected model.
startProtectedModel :: Event b => a -> IO (ProtectedModel a b)
startProtectedModel emptyBM = do
  rm <- atomically $ newTVar $ emptyRM emptyBM
  i  <- forkIO $ dispatcherThread rm
  return ProtectedModel
           { reactiveModel = rm
           , dispatcher    = Just i
           }

-- | Lock the calling thread until the reactive model fulfills a
-- condition.
waitFor :: Event b =>
           ProtectedModel a b -> (ReactiveModelIO a b -> Bool) -> IO ()
waitFor p c = atomically $ void $ do
  rm <- readTVar $ reactiveModel p
  check (c rm)

-- | Run the thread that executes the event handlers.
-- This thread runs indefinitely.
--
-- TODO: would it be better to kill the thread in a clean way
-- (notifying that it has to die ASAP?)
dispatcherThread :: Event b => TVar (ReactiveModelIO a b) -> IO ()
dispatcherThread rmvar = forever $ do
  pa <- atomically $ do
    rm <- readTVar rmvar
    -- Check that there's something pending
    check (not (Seq.null (pendingEvents rm))
           || not (Seq.null (pendingHandlers rm)))
    -- Get the next handler
    let (rm', op) = getPendingHandler rm

    -- Update the ReactiveModel
    writeTVar rmvar rm'

    -- Return the next handler to execute
    return op

  -- Execute the handler
  when (isJust pa) $ fromJust pa

  -- Let other threads run
  yield

-- | Execute an event handler for a given Event.
onEvent :: Event b => ProtectedModel a b -> b -> IO () -> IO ()
onEvent pm ev f = applyToReactiveModel pm (\rm -> RM.onEvent rm ev f)

-- | Execute an event handler for a given Event.
onEvents :: (F.Foldable container, Event b) => ProtectedModel a b -> container b -> IO () -> IO ()
onEvents pm evs f = applyToReactiveModel pm (\rm -> RM.onEvents rm evs f)

-- | Perform a modification to the underlying reactive model.
applyToReactiveModel :: Event b
                        => ProtectedModel a b
                        -> (ReactiveModelIO a b -> ReactiveModelIO a b)
                        -> IO ()
applyToReactiveModel p f = atomically $ onTVar (reactiveModel p) f
  where onTVar v g = readTVar v >>= (writeTVar v . g)

-- | Calculate a value from the reactive model.
onReactiveModel :: Event b
                   => ProtectedModel a b
                   -> (ReactiveModelIO a b -> c)
                   -> IO c
onReactiveModel p f = fmap f $ atomically $ readTVar $ reactiveModel p

-- | Calculate a value from the reactive model and update it at the same time
fromReactiveModel :: Event b
                  => ProtectedModel a b
                  -> (ReactiveModelIO a b -> (ReactiveModelIO a b, c))
                  -> IO c
fromReactiveModel p f = atomically $ do
  rm <- readTVar (reactiveModel p)
  let (rm', v) = f rm
  writeTVar (reactiveModel p) rm'
  return v
