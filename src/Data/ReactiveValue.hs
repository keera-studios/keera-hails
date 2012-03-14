{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- | This is a more general, cleaner interface that allows Model to Model
-- synchronization and view to view.
--
-- It is meant to replace Hails.MVC.Controller.Reactive as soon as
-- we do not need to provide an undefined value for the function
-- reactiveValueOnCanRead.
module Data.ReactiveValue where

class ReactiveValueRead a b where
  reactiveValueOnCanRead :: a -> b -> IO () -> IO ()
  reactiveValueRead :: a -> IO b

class ReactiveValueWrite a b where
  reactiveValueWrite :: a -> b -> IO ()

class (ReactiveValueRead a b, ReactiveValueWrite a b) => ReactiveValueReadWrite a b where

class ReactiveValueActivatable a where
  reactiveValueOnActivate :: a -> IO () -> IO ()

instance ReactiveValueActivatable a => ReactiveValueRead a () where
  reactiveValueOnCanRead act () op = reactiveValueOnActivate act op
  reactiveValueRead _ = return ()

(=:>) v1 v2 = do
  reactiveValueOnCanRead v1 undefined sync1
  where sync1 = reactiveValueRead v1 >>= reactiveValueWrite v2

(<:=) v2 v1 = do
  reactiveValueOnCanRead v1 undefined sync1
  where sync1 = reactiveValueRead v1 >>= reactiveValueWrite v2

(=:=) v1 v2 = do
   reactiveValueOnCanRead v1 undefined sync1
   reactiveValueOnCanRead v2 undefined sync2
  where sync1 = reactiveValueRead v1 >>= reactiveValueWrite v2
        sync2 = reactiveValueRead v2 >>= reactiveValueWrite v1

data TypedReactiveValue a b = TypedReactiveValue a b

instance (ReactiveValueWrite a b) => ReactiveValueWrite (TypedReactiveValue a b) b where
  reactiveValueWrite (TypedReactiveValue x _) v = reactiveValueWrite x v

instance (ReactiveValueRead a b) => ReactiveValueRead (TypedReactiveValue a b) b where
  reactiveValueOnCanRead (TypedReactiveValue x _) v op = (reactiveValueOnCanRead x) v op
  reactiveValueRead (TypedReactiveValue x _)           = reactiveValueRead x
