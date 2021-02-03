-- |
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
--
-- MVars as Reactive Values.
--
-- CBMVars are MVars enriched with a notion of callbacks that are
-- executed when the value in the MVar is altered.
--
-- This module wraps CBMVars into reactive values, making it easy
-- to use them as models of reactive applications.
module Data.CBMVar.Reactive where

import Data.CBMVar        (CBMVar, installCallbackCBMVar, readCBMVar,
                           writeCBMVar)
import Data.ReactiveValue (ReactiveFieldRead(..), ReactiveFieldReadWrite(..))

-- | Return a read-only reactive value wrapping a CBMVar.
cbmvarReactiveRO :: CBMVar a -> ReactiveFieldRead IO a
cbmvarReactiveRO mvar = ReactiveFieldRead getter notifier
  where
    getter     = readCBMVar mvar
    notifier   = installCallbackCBMVar mvar

-- | Return a read-write reactive value wrapping a CBMVar.
cbmvarReactiveRW :: CBMVar a -> ReactiveFieldReadWrite IO a
cbmvarReactiveRW mvar = ReactiveFieldReadWrite setter getter notifier
  where
    setter     = writeCBMVar mvar
    getter     = readCBMVar mvar
    notifier   = installCallbackCBMVar mvar
