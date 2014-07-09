{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
-- | This is a more general, cleaner interface that allows Model to Model
-- synchronization and view to view.
--
-- It is meant to replace Hails.MVC.Controller.Reactive as soon as
-- we do not need to provide an undefined value for the function
-- reactiveValueOnCanRead.
module Data.ReactiveValue where

import Control.Monad
import Control.GFunctor -- Functors parameterised over the morphisms
                        -- in the source category
import Data.Functor.Contravariant

class ReactiveValueRead a b m | a -> b where
  reactiveValueOnCanRead :: a -> m () -> m ()
  reactiveValueRead :: a -> m b

class ReactiveValueWrite a b m where
  reactiveValueWrite :: a -> b -> m ()

reactiveValueModify :: (Monad m, ReactiveValueReadWrite a b m) => a -> (b -> b) -> m ()
reactiveValueModify r f = reactiveValueWrite r . f =<< reactiveValueRead r

class (ReactiveValueRead a b m, ReactiveValueWrite a b m) => ReactiveValueReadWrite a b m

(=:>) :: Monad m => (ReactiveValueRead a b m, ReactiveValueWrite c b m) => a -> c -> m ()
(=:>) v1 v2 = reactiveValueOnCanRead v1 sync1
  where sync1 = reactiveValueRead v1 >>= reactiveValueWrite v2

(<:=) :: Monad m => (ReactiveValueRead a b m, ReactiveValueWrite c b m) => c -> a -> m ()
(<:=) v2 v1 = reactiveValueOnCanRead v1 sync1
  where sync1 = reactiveValueRead v1 >>= reactiveValueWrite v2

(=:=) :: Monad m => (ReactiveValueReadWrite a b m, ReactiveValueReadWrite c b m) => a -> c -> m ()
(=:=) v1 v2 = do
  reactiveValueOnCanRead v1 sync1
  reactiveValueOnCanRead v2 sync2
  where sync1 = reactiveValueRead v1 >>= reactiveValueWrite v2
        sync2 = reactiveValueRead v2 >>= reactiveValueWrite v1

type FieldGetter m a   = m a
type FieldSetter m a   = a -> m ()
type FieldNotifier m a = m () -> m () -- FIXME: why does fieldnotifier have an argument

data ReactiveFieldRead      m a = ReactiveFieldRead (FieldGetter m a) (FieldNotifier m a)
data ReactiveFieldWrite     m a = ReactiveFieldWrite (FieldSetter m a)
data ReactiveFieldReadWrite m a = ReactiveFieldReadWrite (FieldSetter m a) (FieldGetter m a) (FieldNotifier m a)

instance ReactiveValueRead (ReactiveFieldRead m a) a m where
  reactiveValueOnCanRead (ReactiveFieldRead _ notifier) = notifier
  reactiveValueRead (ReactiveFieldRead getter _)        = getter

instance ReactiveValueWrite (ReactiveFieldWrite m a) a m where
  reactiveValueWrite (ReactiveFieldWrite setter) = setter

instance ReactiveValueRead (ReactiveFieldReadWrite m a) a m where
  reactiveValueOnCanRead (ReactiveFieldReadWrite _ _ notifier) = notifier
  reactiveValueRead (ReactiveFieldReadWrite _ getter _)        = getter

instance ReactiveValueWrite (ReactiveFieldReadWrite m a) a m where
  reactiveValueWrite (ReactiveFieldReadWrite setter _ _) = setter

instance ReactiveValueReadWrite (ReactiveFieldReadWrite m a) a m

type ReactiveFieldActivatable m = ReactiveFieldRead m ()

mkActivatable :: Monad m => (m () -> m ()) -> ReactiveFieldActivatable m
mkActivatable f = ReactiveFieldRead getter notifier
 where getter   = return ()
       notifier = f

class ReactiveValueActivatable m a where
   defaultActivation :: a -> ReactiveFieldActivatable m

-- instance (ReactiveValueWrite a b) => ReactiveValueWrite (TypedReactiveValue a b) b where
--   reactiveValueWrite (TypedReactiveValue x _) v = reactiveValueWrite x v
-- 
-- instance (ReactiveValueRead a b) => ReactiveValueRead (TypedReactiveValue a b) b where
--   reactiveValueOnCanRead (TypedReactiveValue x _) v op = (reactiveValueOnCanRead x) v op
--   reactiveValueRead (TypedReactiveValue x _)           = reactiveValueRead x

liftR :: (Monad m, ReactiveValueRead a b m) => a -> (b -> c) -> ReactiveFieldRead m c
liftR e f = ReactiveFieldRead getter notifier
 where notifier = reactiveValueOnCanRead e
       getter   = liftM f (reactiveValueRead e)

liftR2 :: (Monad m, ReactiveValueRead a b m, ReactiveValueRead c d m) => a -> c -> (b -> d -> e) -> ReactiveFieldRead m e
liftR2 e1 e2 f = ReactiveFieldRead getter notifier
  where getter = do v1 <- reactiveValueRead e1
                    v2 <- reactiveValueRead e2
                    return (f v1 v2)
        notifier p = do reactiveValueOnCanRead e1 p
                        reactiveValueOnCanRead e2 p

liftW :: (Monad m, ReactiveValueWrite a b m) => a -> (c -> b) -> ReactiveFieldWrite m c
liftW e f = ReactiveFieldWrite setter
  where setter = reactiveValueWrite e . f

liftW2 :: (Monad m, ReactiveValueWrite a b m, ReactiveValueWrite d e m) => a -> d -> (c -> (b,e)) -> ReactiveFieldWrite m c
liftW2 e1 e2 f = ReactiveFieldWrite setter
  where setter x = do let (v1,v2) = f x
                      reactiveValueWrite e1 v1
                      reactiveValueWrite e2 v2

newtype BijectiveFunc a b = BijectiveFunc (a -> b, b -> a)

type Involution a = BijectiveFunc a a
involution :: (a -> a) -> Involution a
involution f = BijectiveFunc (f, f)

liftRW :: (Monad m, ReactiveValueReadWrite a b m) => a -> BijectiveFunc b c -> ReactiveFieldReadWrite m c
liftRW e (BijectiveFunc (f1, f2)) = ReactiveFieldReadWrite setter getter notifier
  where ReactiveFieldRead getter notifier = liftR e f1
        ReactiveFieldWrite setter         = liftW e f2

liftRW2 :: (Monad m, ReactiveValueReadWrite a b m, ReactiveValueReadWrite c d m) => a -> c -> BijectiveFunc e (b,d) -> ReactiveFieldReadWrite m e
liftRW2 e1 e2 (BijectiveFunc (f1, f2)) = ReactiveFieldReadWrite setter getter notifier
  where ReactiveFieldRead getter notifier = liftR2 e1 e2 (curry f2)
        ReactiveFieldWrite setter         = liftW2 e1 e2 f1

-- Functor definitions
instance (Functor m, Monad m) => Functor (ReactiveFieldRead m) where
  fmap = flip liftR

    -- (ReactiveFieldRead getter notifier) = ReactiveFieldRead (fmap f getter) notifier
instance (Monad m) => Contravariant (ReactiveFieldWrite m) where
  contramap = flip liftW

instance Monad m => GFunctor (ReactiveFieldReadWrite m) BijectiveFunc where
  gmap = flip liftRW

