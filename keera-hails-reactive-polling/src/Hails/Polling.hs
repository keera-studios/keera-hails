-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.Polling where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.CBMVar
import Data.ReactiveValue

pollingReactive :: IO a
                -> Maybe Int
                -> IO (ReactiveFieldRead IO a)
pollingReactive sensor delay = do
  initialV <- sensor
  mvar <- newCBMVar initialV

  forkIO $ forever $ do v <- sensor
                        writeCBMVar mvar v
                        maybe (return ()) (void . threadDelay) delay

  -- RV fields
  let getter   = readCBMVar mvar
      notifier = installCallbackCBMVar mvar

  return $ ReactiveFieldRead getter notifier
