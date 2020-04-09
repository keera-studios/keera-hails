-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013-2020
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
--
module Controller.Conditions where

import Controller.Conditions.Width as Controller.Conditions.Width
import CombinedEnvironment

installHandlers :: CEnv -> IO ()
installHandlers cenv = do
  Controller.Conditions.Width.installHandlers cenv
  return ()
