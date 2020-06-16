-- |
-- Copyright   : (C) Keera Studios Ltd, 2020
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
--
-- Abstract definition of a calculator.
module Data.Calculator
    ( Calculator
    , mkCalculator
    , currentValue
    , addDigit
    , applyAction
    , applyOperator
    )
  where

-- External import
import Data.Maybe (fromMaybe)

-- Internal import
import Data.Action (Action (Clear, Equals))

-- * Calculator

-- | The status of a calculator may be defined by three components:
--
-- One that determines the current calculated value, one that determines the
-- new value being introduced, and one that determines the operator that should
-- be applied.
data Calculator = Calculator
  { calcValue           :: Int
  , calcCurrentInput    :: Maybe Int
  , calcCurrentOperator :: Maybe (Int -> Int -> Int)
  }

-- | Constructor for an initial calculator holding no value or operator.
mkCalculator :: Calculator
mkCalculator = Calculator 0 Nothing Nothing

-- | Value that should be shown in the calculator: the value being introduced,
-- if any, or the last calculation otherwise.
currentValue :: Calculator -> Int
currentValue calc = fromMaybe (calcValue calc) (calcCurrentInput calc)

-- | Add a new digit to the calculator.
addDigit :: Calculator -> Int -> Calculator
addDigit calc d = calc { calcCurrentInput = Just (currentInputValue * 10 + d) }
  where
    currentInputValue = fromMaybe 0 $ calcCurrentInput calc

-- | Given a new operator being introduced, force the calculation of the
-- ongoing calculation (if any), and set the calculator to receive a new value
-- and apply a new operator.
applyOperator :: Calculator -> (Int -> Int -> Int) -> Calculator
applyOperator calc op =
  case (calcCurrentOperator calc, calcCurrentInput calc) of
    (_,        Nothing) -> calc { calcCurrentOperator = Just op }
    (Nothing,  Just y)  -> calc { calcValue           = y
                                , calcCurrentOperator = Just op
                                , calcCurrentInput    = Nothing
                                }
    (Just op', Just y)  -> calc { calcValue           = op' (currentValue calc) y
                                , calcCurrentOperator = Just op
                                , calcCurrentInput    = Nothing
                                }

-- | Given a new operator being introduced, force the calculation of the
-- ongoing calculation (if any), and set the calculator to receive a new value
-- and apply a new operator.
--
-- If only this function is used to update the operator in the calculator,
-- it will ensure that it is always a binary operator and never e.g., Equals.
applyAction :: Calculator -> Action -> Calculator
applyAction calc Equals = calc { calcValue           = y'
                               , calcCurrentOperator = Nothing
                               , calcCurrentInput    = Nothing
                               }
  where
    y' = case calcCurrentOperator calc of
           Nothing -> currentValue calc
           Just op -> op (calcValue calc)  (currentValue calc)

applyAction calc Clear = case calcCurrentInput calc of
  Nothing -> calc { calcValue = 0
                  , calcCurrentOperator = Nothing
                  }
  Just 0  -> calc { calcValue = 0
                  , calcCurrentOperator = Nothing
                  , calcCurrentInput = Nothing
                  }
  Just _  -> calc { calcCurrentInput = Just 0 }
