{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
-- | Protected Reactive Fields
-- 
-- This module defines several classes and operations that are used to
-- create reactive fields and to bind reactive fields in the view to
-- reactive fields in the model.
--
-- FIXME: Due to the restrictions in the type classes, the current
-- version uses Model.ProtectedModel.ProtectedModelInternals.ProtectedModel
-- and will live in Model.ProtectedModel for now. If should be moved
-- to where the generic Control.Concurrent.*.ProtectedModel lives.

module Hails.MVC.Model.ProtectedModel.Reactive where

-- import Hails.MVC.Model.ReactiveModel (FullEvent (FullEvent))
import Hails.MVC.Model.ProtectedModel
import Hails.MVC.Model.ReactiveModel
-- import Model.ProtectedModel.ProtectedModelInternals
-- import Model.ReactiveModel.ModelEvents

type Setter a b c = ProtectedModel b c -> a -> IO()
type Getter a b c = ProtectedModel b c -> IO a

class ReactiveField a b c d | a -> b, a -> c, a -> d where
  events :: a -> [ d ]

class ReactiveField a b c d => ReactiveReadField a b c d where
  getter :: a -> Getter b c d

class ReactiveWriteField a b c d where
  setter :: a -> Setter b c d

class (ReactiveField a b c d, ReactiveReadField a b c d, ReactiveWriteField a b c d) => ReactiveReadWriteField a b c d where

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
