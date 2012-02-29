-- | This is the main program with which the IDE is launched.
--
-- | FIXME: In a very rails-like move, this module will likely be
-- exactly the same for all programs, so we should try to put
-- a "Convention-over-configuration" policy in place and remove this
-- unless it must be adapted by the user.
module Main where

-- Internal imports
import Controller

-- |The IDE starts here. Here we simply call the controller, which
-- takes control from now on.
main :: IO ()
main = startController
