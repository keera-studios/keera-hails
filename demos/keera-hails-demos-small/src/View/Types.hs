-- |
-- Copyright   : (C) Keera Studios Ltd, 2020
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
--
-- Abstract view representation as a collection of reactive values.
module View.Types where

import Data.ReactiveValue (ReactiveFieldRead, ReactiveFieldReadWrite)

import Data.Action (Action)

-- | A UI is a collection of reactive values. For this calculator example,
-- we will use one readable value for each number, one for each operator, and a
-- read-write RV for the text field where the results have to be shown.
data UI = UI
  { numbers   :: [ReactiveFieldRead IO Int]                 -- 0 to 9
  , operators :: [ReactiveFieldRead IO (Int -> Int -> Int)] -- plus, minus, times, div
  , actions   :: [ReactiveFieldRead IO Action]              -- plus, minus, times, div
  , textField :: ReactiveFieldReadWrite IO String
  }
