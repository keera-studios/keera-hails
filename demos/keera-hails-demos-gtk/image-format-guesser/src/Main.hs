-- | This is the main program with which the IDE is launched.
module Main where

-- Internal imports
import Controller

-- |The IDE starts here. Here we simply call the controller, which takes control
-- from now on.
main :: IO ()
main = startController
