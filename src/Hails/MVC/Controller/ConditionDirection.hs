-- | Conditions to update a program can go from the view to the model
-- and from the model to the view. Future versions must "ignore" this
-- completely and simply work even if the constraints are of kind 
-- Model-to-Model or View-to-View
--
-- FIXME: In a very rails-like move, this module will likely be
-- exactly the same for all programs, so we should try to put
-- a "Convention-over-configuration" policy in place and remove this
-- unless it must be adapted by the user.
module Hails.MVC.Controller.ConditionDirection where

data ConditionDirection = VM
                        | MV

