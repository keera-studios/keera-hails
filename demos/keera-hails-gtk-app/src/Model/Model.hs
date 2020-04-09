-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013-2020
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
--
module Model.Model where

data Model = Model
  {
    modelWidth :: Int
  }

emptyBM :: Model
emptyBM = Model 0
