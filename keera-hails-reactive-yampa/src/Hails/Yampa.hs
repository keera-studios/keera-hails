-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE FlexibleInstances    #-}
module Hails.Yampa where

import Control.Monad (when, void)
import Data.CBMVar
import Data.IORef
import Data.Maybe (fromJust)
import Data.ReactiveValue
import Data.Time.Clock
import FRP.Yampa

-- | Define a couple of RVs connected by an SF, so that
-- writing to one makes the SF process the value and make
-- the result available in a readable RV.
yampaReactiveDual :: a -> SF a b
                  -> IO (ReactiveFieldWrite IO a, ReactiveFieldRead IO b)
yampaReactiveDual initial sf = do 
  mvar <- newCBMVar Nothing

  -- Initial clock
  initialClock <- getCurrentTime
  lastTimeR    <- newIORef initialClock

  -- Reactimation Handle
  rh <- reactInit
          -- Initial value
          (return initial)
          -- Actuation
          (\_ changed output -> do when changed $ writeCBMVar mvar (Just output)
                                   return False
          )
          -- Processing
          sf

  -- RV fields
  let getter   = fmap fromJust $ readCBMVar mvar
      setter y = void $ do -- Calculate time delta
                           newTime  <- getCurrentTime
                           lastTime <- readIORef lastTimeR
                           let dt = realToFrac $ diffUTCTime newTime lastTime
                           writeIORef lastTimeR newTime

                           -- Run FRP system
                           react rh (dt, Just y)

      notifier evH = installCallbackCBMVar mvar evH

  let rvRead  = ReactiveFieldRead getter notifier
      rvWrite = ReactiveFieldWrite setter
                          
  return (rvWrite, rvRead)

-- | Create an RV that processes the value
-- with an SF every time it is written.
yampaReactive :: a -> SF a a -> IO (ReactiveFieldReadWrite IO a)
yampaReactive initial sf = do 
  mvar <- newCBMVar initial

  -- Initial clock
  initialClock <- getCurrentTime
  lastTimeR    <- newIORef initialClock

  -- Reactimation Handle
  rh <- reactInit
          -- Initial value
          (readCBMVar mvar)
          -- Actuation
          (\_ changed output -> do when changed $ writeCBMVar mvar output
                                   return False
          )
          -- Processing
          sf

  -- RV fields
  let getter   = readCBMVar mvar
      setter y = void $ do -- Calculate time delta
                           newTime  <- getCurrentTime
                           lastTime <- readIORef lastTimeR
                           let dt = realToFrac $ diffUTCTime newTime lastTime
                           writeIORef lastTimeR newTime

                           -- Run FRP system
                           react rh (dt, Just y)

      notifier = installCallbackCBMVar mvar
                          
  return $ ReactiveFieldReadWrite setter getter notifier

-- | Alternative (simpler) definition to 'yampaReactive'
yampaReactive' :: a -> SF a a -> IO (ReactiveFieldReadWrite IO a)
yampaReactive' initial sf = do
  (rvW, rvR) <- yampaReactiveDual initial sf
  return $ combineRVReadWrite rvR rvW

-- | To be moved to Data.ReactiveValue
combineRVReadWrite :: (ReactiveValueRead r1 a m,  ReactiveValueWrite r2 a m)
                   => r1 -> r2 -> ReactiveFieldReadWrite m a
combineRVReadWrite rvR rvW = ReactiveFieldReadWrite setter getter notifier
  where getter   = reactiveValueRead rvR
        setter   = reactiveValueWrite rvW
        notifier = reactiveValueOnCanRead rvR
                          
