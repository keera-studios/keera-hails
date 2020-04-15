-- | This is the main program with which the IDE is launched.
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Main where

-- Internal imports
import Controller

-- |The IDE starts here. Here we simply call the controller, which takes control
-- from now on.
main :: IO ()
main = startController
