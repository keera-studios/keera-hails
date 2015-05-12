{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
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

-- * Reactive values: common interface

-- | Readable reactive values
class ReactiveValueRead a b m | a -> b where
  reactiveValueOnCanRead :: a -> m () -> m ()
  reactiveValueRead :: a -> m b

-- | Writable reactive values
class ReactiveValueWrite a b m where
  reactiveValueWrite :: a -> b -> m ()

-- | Read-write reactive values
class (ReactiveValueRead a b m, ReactiveValueWrite a b m) => ReactiveValueReadWrite a b m

-- * Reactive rules (data dependency/passing building combinators)

-- | Priorities so that we can write them infix without parenthesising
infix 9 =:=
infix 9 =:>
infix 9 <:=

-- | Left to right
(=:>) :: Monad m => (ReactiveValueRead a b m, ReactiveValueWrite c b m) => a -> c -> m ()
(=:>) v1 v2 = reactiveValueOnCanRead v1 sync1
  where sync1 = reactiveValueRead v1 >>= reactiveValueWrite v2

-- | Right-to-left
(<:=) :: Monad m => (ReactiveValueRead a b m, ReactiveValueWrite c b m) => c -> a -> m ()
(<:=) v2 v1 = reactiveValueOnCanRead v1 sync1
  where sync1 = reactiveValueRead v1 >>= reactiveValueWrite v2

-- | Bidirectional
(=:=) :: Monad m => (ReactiveValueReadWrite a b m, ReactiveValueReadWrite c b m) => a -> c -> m ()
(=:=) v1 v2 = do
  -- This is often async, so the fact that one comes before the other does not guarantee
  -- that they will be refreshed in that order.
  v1 =:> v2
  v1 <:= v2
  -- reactiveValueOnCanRead v1 sync1
  -- reactiveValueOnCanRead v2 sync2
  -- where sync1 = reactiveValueRead v1 >>= reactiveValueWrite v2
  --       sync2 = reactiveValueRead v2 >>= reactiveValueWrite v1

-- * Purely functional implementation

-- ** Setters, getters and notifiers
type FieldGetter m a   = m a
type FieldSetter m a   = a -> m ()
type FieldNotifier m a = m () -> m () -- FIXME: why does fieldnotifier have an argument

-- ** Concrete types implementing the above interface
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

-- ** Activatable reactive values (producing units)
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

-- * Creating RVs based on other RVs

-- ** Lifting onto readable values
liftR :: (Monad m, ReactiveValueRead a b m) => a -> (b -> c) -> ReactiveFieldRead m c
liftR e f = ReactiveFieldRead getter notifier
 where notifier = reactiveValueOnCanRead e
       getter   = liftM f (reactiveValueRead e)

liftR2 :: (Monad m, ReactiveValueRead a b m, ReactiveValueRead c d m)
       => a -> c -> (b -> d -> e) -> ReactiveFieldRead m e
liftR2 e1 e2 f = ReactiveFieldRead getter notifier
  where getter = do v1 <- reactiveValueRead e1
                    v2 <- reactiveValueRead e2
                    return (f v1 v2)
        notifier p = do reactiveValueOnCanRead e1 p
                        reactiveValueOnCanRead e2 p

-- Same as lifting join . f?
liftMR :: (Monad m, ReactiveValueRead a b m) => a -> (b -> m c) -> ReactiveFieldRead m c
liftMR e f = ReactiveFieldRead getter notifier
 where notifier = reactiveValueOnCanRead e
       getter   = f =<< reactiveValueRead e

-- ** Lifting onto writeable values
liftW :: (Monad m, ReactiveValueWrite a b m)
      => a -> (c -> b) -> ReactiveFieldWrite m c
liftW e f = ReactiveFieldWrite setter
  where setter = reactiveValueWrite e . f

liftW2 :: (Monad m, ReactiveValueWrite a b m, ReactiveValueWrite d e m)
       => a -> d -> (c -> (b,e)) -> ReactiveFieldWrite m c
liftW2 e1 e2 f = ReactiveFieldWrite setter
  where setter x = do let (v1,v2) = f x
                      reactiveValueWrite e1 v1
                      reactiveValueWrite e2 v2

liftMW :: (Monad m, ReactiveValueWrite a b m)
       => a -> (c -> m b) -> ReactiveFieldWrite m c
liftMW e f = ReactiveFieldWrite setter
  where setter x = reactiveValueWrite e =<< f x

readOnly :: ReactiveValueRead r a m => r -> ReactiveFieldRead m a
readOnly r = ReactiveFieldRead (reactiveValueRead r) (reactiveValueOnCanRead r)

writeOnly :: ReactiveValueWrite r a m => r -> ReactiveFieldWrite m a
writeOnly r = ReactiveFieldWrite (reactiveValueWrite r)

-- * Lift monadic operations
wrapMW :: (a -> m ()) -> ReactiveFieldWrite m a
wrapMW f = ReactiveFieldWrite f

wrapMRPassive :: Monad m => m a -> ReactiveFieldRead m a
wrapMRPassive f = ReactiveFieldRead f (const (return ()))

wrapMR :: m a -> (m () -> m ()) -> ReactiveFieldRead m a
wrapMR f p = ReactiveFieldRead f p

-- ** Lifting onto read-write values

-- *** Bijections
newtype BijectiveFunc a b = BijectiveFunc
  { unBijectiveFunc :: (a -> b, b -> a) }

bijection :: (a -> b, b -> a) -> BijectiveFunc a b
bijection = BijectiveFunc

direct :: BijectiveFunc a b -> (a -> b)
direct = fst . unBijectiveFunc

inverse :: BijectiveFunc a b -> (b -> a)
inverse = snd . unBijectiveFunc

type Involution a = BijectiveFunc a a
involution :: (a -> a) -> Involution a
involution f = BijectiveFunc (f, f)

-- *** Actual lifting
liftRW :: (Monad m, ReactiveValueReadWrite a b m)
       => a -> BijectiveFunc b c -> ReactiveFieldReadWrite m c
liftRW e (BijectiveFunc (f1, f2)) = ReactiveFieldReadWrite setter getter notifier
  where ReactiveFieldRead getter notifier = liftR e f1
        ReactiveFieldWrite setter         = liftW e f2

liftRW2 :: (Monad m, ReactiveValueReadWrite a b m, ReactiveValueReadWrite c d m)
        => a -> c -> BijectiveFunc e (b,d) -> ReactiveFieldReadWrite m e
liftRW2 e1 e2 (BijectiveFunc (f1, f2)) = ReactiveFieldReadWrite setter getter notifier
  where ReactiveFieldRead getter notifier = liftR2 e1 e2 (curry f2)
        ReactiveFieldWrite setter         = liftW2 e1 e2 f1

pairRW :: (Monad m,
           ReactiveValueReadWrite a b m,
           ReactiveValueReadWrite c d m)
       => a -> c -> ReactiveFieldReadWrite m (b, d)
pairRW a b = liftRW2 a b (bijection (id, id))

{-# INLINE eqCheck #-}
eqCheck :: (Eq v, Monad m) => ReactiveFieldReadWrite m v -> ReactiveFieldReadWrite m v
eqCheck (ReactiveFieldReadWrite setter getter notifier) = ReactiveFieldReadWrite setter' getter notifier
 where setter' v = do o <- getter
                      when (o /= v) $ setter v

-- ** Modifying reactive values (applying modification transformations)

-- | Lifting modification functions
modRW :: (Monad m, ReactiveValueReadWrite a b m)
      => (b -> c -> b) -> a -> ReactiveFieldWrite m c
modRW f rv = ReactiveFieldWrite setter
  where setter c = do b <- reactiveValueRead rv
                      let b' = f b c
                      reactiveValueWrite rv b'

reactiveValueModify :: (Monad m, ReactiveValueReadWrite a b m) => a -> (b -> b) -> m ()
reactiveValueModify r f = reactiveValueWrite r . f =<< reactiveValueRead r

-- * Deactivating reactive values

-- | Turning an active RV into a passive one (does not propagate changes)
-- Note that this does not really affect the RV itself, only produces a new
-- RV that will not propagate changes. So, if used in a reactive relation,
-- values will not get propagated when they change. It is useful in combination
-- with lifts, to achieve things similar to Yampa's tagging, but this might
-- be more general.
passivelyR :: (Monad m, ReactiveValueRead a b m)
           => a -> ReactiveFieldRead m b
passivelyR rv =
  ReactiveFieldRead (reactiveValueRead rv) (\_ -> return ())

passivelyRW :: (Monad m, ReactiveValueReadWrite a b m)
            => a -> ReactiveFieldReadWrite m b
passivelyRW rv =
  ReactiveFieldReadWrite (reactiveValueWrite rv) (reactiveValueRead rv) (\_ -> return ())

-- * Conditionals

-- Check condition and notify only when holds
ifRW_ :: (Monad m, ReactiveValueRead c Bool m, ReactiveValueReadWrite v a m)
      => c -> v
      -> ReactiveFieldReadWrite m a
ifRW_ c r = ReactiveFieldReadWrite setter getter notifier
  where setter x   = reactiveValueWrite r x
        getter     = reactiveValueRead r
        -- If either changes, the value *may* be propagated
        notifier p = do reactiveValueOnCanRead c (when' p)
                        reactiveValueOnCanRead r (when' p)

        -- Propagate only if the condition holds
         where when' m = do x <- reactiveValueRead c
                            when x m

-- Check condition, and write or notify only when it holds
ifRW :: (Monad m, ReactiveValueRead c Bool m, ReactiveValueReadWrite v a m)
     => c -> v
     -> ReactiveFieldReadWrite m a
ifRW c r = ReactiveFieldReadWrite setter getter notifier
  where setter x   = do b <- reactiveValueRead c
                        when b $
                          reactiveValueWrite r x
        getter     = reactiveValueRead r
        -- If either changes, the value *may* be propagated
        notifier p = do reactiveValueOnCanRead c (when' p)
                        reactiveValueOnCanRead r (when' p)

        -- Propagate only if the condition holds
         where when' m = do b <- reactiveValueRead c
                            when b m

-- * Category theoretic definitions

-- Functor definitions
instance (Functor m, Monad m) => Functor (ReactiveFieldRead m) where
  fmap = flip liftR

-- FIXME: I might not want to provide this: the contravariant library
-- depends on transformers.
-- (ReactiveFieldRead getter notifier) = ReactiveFieldRead (fmap f getter) notifier
instance (Monad m) => Contravariant (ReactiveFieldWrite m) where
  contramap = flip liftW

instance Monad m => GFunctor (ReactiveFieldReadWrite m) BijectiveFunc where
  gmap = flip liftRW

-- | Temporary: will be moved to Keera Hails' Reactive Values library.
governingR :: (ReactiveValueRead a b m,  ReactiveValueRead c d m)
           => a -> c -> ReactiveFieldRead m d
governingR r c = ReactiveFieldRead getter notifier
  where getter   = reactiveValueRead c
        notifier = reactiveValueOnCanRead r
