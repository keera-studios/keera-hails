-- | Conditions to update a program can go from the view to the model
-- and from the model to the view. Future versions must "ignore" this
-- completely and simply work even if the constraints are of kind 
-- Model-to-Model or View-to-View

module Hails.MVC.Controller.ConditionDirection where

data ConditionDirection = VM
                        | MV

