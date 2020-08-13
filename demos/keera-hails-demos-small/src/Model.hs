-- |
-- Copyright   : (C) Keera Studios Ltd, 2020
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
--
-- A reactive model for a Keera Hails application needs to offer
-- two things: an abstract data type that encapsulates the model,
-- and a reactive API in the form of read/write/read-write reactive
-- values that enable interaction with the model in a thread-safe
-- way.
--
-- There are two kinds of RVs: information end points, and action
-- end points. The former must be readable (at least), and the latter
-- must be writable. Examples of the former are portions of the
-- model that can be reactively modified from the outside. Examples
-- of the latter are end points for actions on the model.
module Model
    ( Model
    , reactiveModel
    , modelAddDigit
    , modelApplyAction
    , modelApplyOperator
    , modelValue
    )
  where

import Data.Maybe         (fromMaybe)

import Data.CBMVar          (newCBMVar)
import Data.CBMVar.Reactive (cbmvarReactiveRW)
import Data.ReactiveValue   (ReactiveFieldRead, ReactiveFieldWrite, (<^>), modRW)

import Data.Calculator (Calculator, addDigit, applyAction, applyOperator,
                        currentValue, mkCalculator)
import Data.Action (Action)

-- | Abstract data type for the application's model.
--
-- It consists of three end points: one to communicatate that new numbers
-- are being introduced, one to communicate that an operator is being
-- introduced, and one to see the current value in the calculator.
data Model = Model
  { modelAddDigit      :: ReactiveFieldWrite IO Int
  , modelApplyOperator :: ReactiveFieldWrite IO (Int -> Int -> Int)
  , modelApplyAction   :: ReactiveFieldWrite IO Action
  , modelValue         :: ReactiveFieldRead  IO Int
  }

-- | Constructor for the reactive model.
--
-- The implementation must guarantee that the access end points to
-- the model are thread safe.
reactiveModel :: IO Model
reactiveModel = do

  -- We use a CBMVar, which is an MVar with call-backs.
  modelCB <- newCBMVar mkCalculator
  let model = cbmvarReactiveRW modelCB

  let addDig = addDigit      `modRW` model
      appOp  = applyOperator `modRW` model
      appAct = applyAction   `modRW` model
      val    = currentValue <^> model

  return $ Model addDig appOp appAct val
