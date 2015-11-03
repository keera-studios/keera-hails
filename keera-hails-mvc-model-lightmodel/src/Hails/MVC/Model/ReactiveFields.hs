module Hails.MVC.Model.ReactiveFields where

import Hails.MVC.Model.ReactiveModel

-- TODO: With the new reactive lenses interface,
-- this should uses lenses instead. A 'Field' is
-- just a lens, augmented with an event and a
-- precondition checker. 

-- The following code presents a possibly simpler way of creating reactive
-- fields in a reactive model.
type Field a b c = (b -> a, a -> b -> Bool, a -> b -> b, c)

preTrue :: a -> b -> Bool
preTrue _ _ = True

fieldSetter :: (Eq a, Event c) =>
               Field a b c -> ReactiveModel b c d -> a -> ReactiveModel b c d
fieldSetter f@(_, pre, rSet, ev) rm newVal
  | fieldGetter f rm == newVal       = rm
  | not $ pre newVal $ basicModel rm = triggerEvent rm ev
  | otherwise                        = triggerEvent rm' ev
 where rm' = rm `onBasicModel` rSet newVal

fieldGetter :: (Event c) => Field a b c -> ReactiveModel b c d -> a
fieldGetter (rGet,_,_,_) = rGet . basicModel
