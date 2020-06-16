-- |
-- Copyright   : (C) Keera Studios Ltd, 2020
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
--
-- Abstract definition of an operator button in a calculator.
module Data.Action where

-- | Operator button in a calculator. We support both binary number
-- operators and other operatots that whould return the final value.
data Action = Equals | Clear
  deriving Eq
