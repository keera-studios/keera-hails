-----------------------------------------------------------------------------
-- |
-- Module      :  Main (hlint)
-- Copyright   :  (C) 2015 Ivan Perez, 2013-2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ivan Perez <ivan.perez@keera.co.uk>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module runs HLint on the source tree.
-----------------------------------------------------------------------------
module Main where

import Control.Monad
import Language.Haskell.HLint
import System.Environment
import System.Exit

main :: IO ()
main = do
  args  <- getArgs
  hints <- hlint $ ["src"] ++ args
  unless (null hints) exitFailure
