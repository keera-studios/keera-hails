{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
-- | This is a more general, cleaner interface that allows Model to Model
-- synchronization and view to view.
--
-- It is meant to replace Hails.MVC.Controller.Reactive as soon as
-- we do not need to provide an undefined value for the function
-- reactiveValueOnCanRead.
module Data.ReactiveValue where

import Control.Monad

class ReactiveValueRead a b | a -> b where
  reactiveValueOnCanRead :: a -> IO () -> IO ()
  reactiveValueRead :: a -> IO b

class ReactiveValueWrite a b where
  reactiveValueWrite :: a -> b -> IO ()

class (ReactiveValueRead a b, ReactiveValueWrite a b) => ReactiveValueReadWrite a b

(=:>) :: (ReactiveValueRead a b, ReactiveValueWrite c b) => a -> c -> IO ()
(=:>) v1 v2 = do
  reactiveValueOnCanRead v1 sync1
  where sync1 = reactiveValueRead v1 >>= reactiveValueWrite v2

(<:=) :: (ReactiveValueRead a b, ReactiveValueWrite c b) => c -> a -> IO ()
(<:=) v2 v1 = do
  reactiveValueOnCanRead v1 sync1
  where sync1 = reactiveValueRead v1 >>= reactiveValueWrite v2

(=:=) :: (ReactiveValueReadWrite a b, ReactiveValueReadWrite c b) => a -> c -> IO ()
(=:=) v1 v2 = do
  reactiveValueOnCanRead v1 sync1
  reactiveValueOnCanRead v2 sync2
  where sync1 = reactiveValueRead v1 >>= reactiveValueWrite v2
        sync2 = reactiveValueRead v2 >>= reactiveValueWrite v1

type FieldGetter a   = IO a
type FieldSetter a   = a -> IO ()
type FieldNotifier a = IO () -> IO () -- FIXME: why does fieldnotifier have an argument

data ReactiveFieldRead      a = ReactiveFieldRead (FieldGetter a) (FieldNotifier a)
data ReactiveFieldWrite     a = ReactiveFieldWrite (FieldSetter a)
data ReactiveFieldReadWrite a = ReactiveFieldReadWrite (FieldSetter a) (FieldGetter a) (FieldNotifier a)

instance ReactiveValueRead (ReactiveFieldRead a) a where
  reactiveValueOnCanRead (ReactiveFieldRead _ notifier) = notifier
  reactiveValueRead (ReactiveFieldRead getter _)        = getter

instance ReactiveValueWrite (ReactiveFieldWrite a) a where
  reactiveValueWrite (ReactiveFieldWrite setter) = setter

instance ReactiveValueRead (ReactiveFieldReadWrite a) a where
  reactiveValueOnCanRead (ReactiveFieldReadWrite _ _ notifier) = notifier
  reactiveValueRead (ReactiveFieldReadWrite _ getter _)        = getter

instance ReactiveValueWrite (ReactiveFieldReadWrite a) a where
  reactiveValueWrite (ReactiveFieldReadWrite setter _ _) = setter

instance ReactiveValueReadWrite (ReactiveFieldReadWrite a) a

type ReactiveFieldActivatable = ReactiveFieldRead ()

mkActivatable :: (IO () -> IO ()) -> ReactiveFieldActivatable
mkActivatable f = ReactiveFieldRead getter notifier
 where getter   = return ()
       notifier = f

class ReactiveValueActivatable a where
   defaultActivation :: a -> ReactiveFieldActivatable

-- instance (ReactiveValueWrite a b) => ReactiveValueWrite (TypedReactiveValue a b) b where
--   reactiveValueWrite (TypedReactiveValue x _) v = reactiveValueWrite x v
-- 
-- instance (ReactiveValueRead a b) => ReactiveValueRead (TypedReactiveValue a b) b where
--   reactiveValueOnCanRead (TypedReactiveValue x _) v op = (reactiveValueOnCanRead x) v op
--   reactiveValueRead (TypedReactiveValue x _)           = reactiveValueRead x

liftR :: (ReactiveValueRead a b) => a -> (b -> c) -> ReactiveFieldRead c
liftR e f = ReactiveFieldRead getter notifier
 where notifier = reactiveValueOnCanRead e
       getter   = liftM f (reactiveValueRead e)

liftR2 :: (ReactiveValueRead a b, ReactiveValueRead c d) => a -> c -> (b -> d -> e) -> ReactiveFieldRead e
liftR2 e1 e2 f = ReactiveFieldRead getter notifier
  where getter = do v1 <- reactiveValueRead e1
                    v2 <- reactiveValueRead e2
                    return (f v1 v2)
        notifier p = do reactiveValueOnCanRead e1 p
                        reactiveValueOnCanRead e2 p

liftW :: (ReactiveValueWrite a b) => a -> (c -> b) -> ReactiveFieldWrite c
liftW e f = ReactiveFieldWrite setter
  where setter = reactiveValueWrite e . f

liftW2 :: (ReactiveValueWrite a b, ReactiveValueWrite d e) => a -> d -> (c -> (b,e)) -> ReactiveFieldWrite c
liftW2 e1 e2 f = ReactiveFieldWrite setter
  where setter x = do let (v1,v2) = f x
                      reactiveValueWrite e1 v1
                      reactiveValueWrite e2 v2

newtype BijectiveFunc a b = BijectiveFunc (a -> b, b -> a)

type Involution a = BijectiveFunc a a
involution :: (a -> a) -> Involution a
involution f = BijectiveFunc (f, f)

liftRW :: (ReactiveValueReadWrite a b) => a -> BijectiveFunc b c -> ReactiveFieldReadWrite c
liftRW e (BijectiveFunc (f1, f2)) = ReactiveFieldReadWrite setter getter notifier
  where ReactiveFieldRead getter notifier = liftR e f1
        ReactiveFieldWrite setter         = liftW e f2

liftRW2 :: (ReactiveValueReadWrite a b, ReactiveValueReadWrite c d) => a -> c -> BijectiveFunc e (b,d) -> ReactiveFieldReadWrite e
liftRW2 e1 e2 (BijectiveFunc (f1, f2)) = ReactiveFieldReadWrite setter getter notifier
  where ReactiveFieldRead getter notifier = liftR2 e1 e2 (curry f2)
        ReactiveFieldWrite setter         = liftW2 e1 e2 f1
