{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
-- | Protected Reactive Fields
-- 
-- This module defines several classes and operations that are used to
-- create reactive fields and to bind reactive fields in the view to
-- reactive fields in the model.
--
-- FIXME: Due to the restrictions in the type classes, the current
-- version uses Model.ProtectedModel.ProtectedModelInternals.ProtectedModel.

module Hails.MVC.Model.ProtectedModel.Reactive where

import Data.ReactiveValue
import Hails.MVC.Model.ProtectedModel
import Hails.MVC.Model.ReactiveModel.Events
import Hails.MVC.Model.ReactiveModel hiding (onEvent)

type Setter a b c = ProtectedModel b c -> a -> IO()
type Getter a b c = ProtectedModel b c -> IO a
type Modifier a b c = ProtectedModel b c -> (a -> a) -> IO()
type ModifierIO a b c = ProtectedModel b c -> (a -> IO a) -> IO()

class ReactiveField a b c d | a -> b, a -> c, a -> d where
  events    :: a -> [ d ]

onChanged :: (Event d, ReactiveField a b c d) => ProtectedModel c d -> a -> IO () -> IO ()
onChanged pm field p = mapM_ (\e -> onEvent pm e p) (events field)

class ReactiveField a b c d => ReactiveReadField a b c d where
  getter :: a -> Getter b c d

class ReactiveWriteField a b c d where
  setter :: a -> Setter b c d

class (ReactiveField a b c d, ReactiveReadField a b c d, ReactiveWriteField a b c d) => ReactiveReadWriteField a b c d where

  modifier :: a -> Modifier b c d
  modifier x pm f = do
    v <- getter x pm
    let v' = f v
    setter x pm v'

  modifierIO :: a -> ModifierIO b c d
  modifierIO x pm f = do
    v  <- getter x pm
    v' <- f v
    setter x pm v'

data Event c => ReactiveElement a b c = ReactiveElement
  { reEvents :: [ c ]
  , reSetter :: Setter a b c
  , reGetter :: Getter a b c
  }

instance Event c => ReactiveField (ReactiveElement a b c) a b c where
 events = reEvents

instance Event c => ReactiveReadField (ReactiveElement a b c) a b c where
 getter = reGetter

instance Event c => ReactiveWriteField (ReactiveElement a b c) a b c where
 setter = reSetter

instance Event c => ReactiveReadWriteField (ReactiveElement a b c) a b c where

type FieldAccessor a b c = ProtectedModel b c -> ReactiveFieldReadWrite a

mkFieldAccessor :: InitialisedEvent c => ReactiveElement a b c -> ProtectedModel b c -> ReactiveFieldReadWrite a
mkFieldAccessor (ReactiveElement evs setter' getter') pm = ReactiveFieldReadWrite setter getter notifier
  where setter     = setter' pm
        getter     = getter' pm
        notifier p = onEvents pm (initialisedEvent : evs) p 
