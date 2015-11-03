{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Reactive Values are typed mutable variables with a change notification
-- mechanism.
--
-- They are defined by providing a way to read the value, a way to change it,
-- and a way to install an event listener when the value has changed.
--
-- RVs are pruposely defined in an abstract way, as a type class. GUI toolkits,
-- for instance, can use existing event-handling installing mechanisms to
-- enclose widget attributes as Reactive Values, without the need for an
-- extra layer.
--
-- RVs are complemented with Relation-building functions, which
-- enable pairing RVs during execution so that they are kept in
-- sync for the duration of the program.
--
-- This module only defines RVs and operations on them. For connections
-- to existing backends (GUIs, devices, files, network, FRP), see
-- https://github.com/keera-studios/keera-hails
--
module Data.ReactiveValue where

import Control.Monad
import Control.GFunctor -- Functors parameterised over the morphisms
                        -- in the source category
import Data.Functor.Contravariant

-- * Reactive values: common interface for all RVs

-- | Readable reactive values
class ReactiveValueRead a b m | a -> b, a -> m where
  reactiveValueOnCanRead :: a -> m () -> m ()
  reactiveValueRead :: a -> m b

instance ReactiveValueRead (IO a) a IO where
  reactiveValueOnCanRead _ _ = return ()
  reactiveValueRead      m = m

-- | Writable reactive values
class ReactiveValueWrite a b m | a -> b, a -> m where
  reactiveValueWrite :: a -> b -> m ()

instance ReactiveValueWrite (IO a) () IO where
  reactiveValueWrite m _ = void m

-- | Read-write reactive values
class (ReactiveValueRead a b m, ReactiveValueWrite a b m) => ReactiveValueReadWrite a b m

-- * Reactive rules (data dependency/passing building combinators)

-- | Priorities so that we can write them infix without parenthesising
infix 9 =:=
infix 9 =:>
infix 9 <:=

-- | Left to right RV synchronisation function. If the value on the left
-- changes, the one on the right is updated accordingly.
(=:>) :: Monad m => (ReactiveValueRead a b m, ReactiveValueWrite c b m) => a -> c -> m ()
(=:>) v1 v2 = reactiveValueOnCanRead v1 sync1
  where sync1 = reactiveValueRead v1 >>= reactiveValueWrite v2

-- | Right-to-left RV synchronisation function. If the value on the right
-- changes, the one on the left is updated accordingly.
(<:=) :: Monad m => (ReactiveValueRead a b m, ReactiveValueWrite c b m) => c -> a -> m ()
(<:=) v2 v1 = reactiveValueOnCanRead v1 sync1
  where sync1 = reactiveValueRead v1 >>= reactiveValueWrite v2

-- | Bidirectional synchronisation. When either value changes, the other
-- is updated accordingly.
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

-- * Purely functional implementation of RVs.
--
-- These are used internally for combinators that need to return RV instances. They can
-- also be used to write new backends and library extensions, but they are not
-- recommended to enclose application models. For that purpose, see light models and
-- protected models instead.

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
constR :: Monad m => a ->  ReactiveFieldRead m a
constR e = ReactiveFieldRead getter notifier
 where notifier _ = return ()
       getter     = return e

-- | TODO: Bad name. Should be eliminated or extended with a setter.
initRW :: Monad m => a ->  ReactiveFieldRead m a
initRW e = ReactiveFieldRead getter notifier
 where notifier _ = return ()
       getter     = return e

-- ** Lifting onto readable values

-- | Lift a transformation onto a RV. Note that this creates a new
-- RV, it does not modify the existing RV.
liftR :: (Monad m, ReactiveValueRead a b m) => (b -> c) -> a -> ReactiveFieldRead m c
liftR f e = ReactiveFieldRead getter notifier
 where notifier = reactiveValueOnCanRead e
       getter   = liftM f (reactiveValueRead e)

-- | Shorter name for 'liftR'
(<^>) :: (Monad m, ReactiveValueRead a b m) => (b -> c) -> a -> ReactiveFieldRead m c
(<^>) = liftR

-- | Lift a transformation onto two RVs. Note that this creates a new
-- RV, it does not modify the existing RVs. When either RV changes,
-- the new one triggers a change.
liftR2 :: (Monad m, ReactiveValueRead a b m, ReactiveValueRead c d m)
       => (b -> d -> e) -> a -> c -> ReactiveFieldRead m e
liftR2 f e1 e2 = ReactiveFieldRead getter notifier
  where getter = do v1 <- reactiveValueRead e1
                    v2 <- reactiveValueRead e2
                    return (f v1 v2)
        notifier p = do reactiveValueOnCanRead e1 p
                        reactiveValueOnCanRead e2 p

-- | Lift a transformation onto three RVs. Note that this creates a new
-- RV, it does not modify the existing RVs. When either RV changes,
-- the new one triggers a change.
liftR3 :: ( Monad m, ReactiveValueRead a b m, ReactiveValueRead c d m
          , ReactiveValueRead e f m)
       => (b -> d -> f -> g) -> a -> c -> e -> ReactiveFieldRead m g
liftR3 f e1 e2 e3 = ReactiveFieldRead getter notifier
  where getter = do v1 <- reactiveValueRead e1
                    v2 <- reactiveValueRead e2
                    v3 <- reactiveValueRead e3
                    return (f v1 v2 v3)
        notifier p = do reactiveValueOnCanRead e1 p
                        reactiveValueOnCanRead e2 p
                        reactiveValueOnCanRead e3 p

-- | Lift a parameterised monadic transformation onto an RV.
--
-- Same as lifting join . f?
liftMR :: (Monad m, ReactiveValueRead a b m) => (b -> m c) -> a -> ReactiveFieldRead m c
liftMR f e = ReactiveFieldRead getter notifier
 where notifier = reactiveValueOnCanRead e
       getter   = f =<< reactiveValueRead e

-- ** Lifting onto writeable values

-- | Create a constant writable RV.
--
constW :: (Monad m, ReactiveValueWrite v a m) => a -> v -> ReactiveFieldWrite m b
constW c v = ReactiveFieldWrite $ \_ -> reactiveValueWrite v c

-- | Lift a transformation onto an RV. This creates a new RV, it does
-- not actually modify the old RV (when this one is written to, so will
-- be the old one, but both will keep existing somewhat independently).
liftW :: (Monad m, ReactiveValueWrite a b m)
      => (c -> b) -> a -> ReactiveFieldWrite m c
liftW f e = ReactiveFieldWrite setter
  where setter = reactiveValueWrite e . f

-- | Lift a transformation onto two RVs. This creates a new RV, it does
-- not actually modify the old RVs (when this one is written to, so will
-- be the old ones, but both will keep existing somewhat independently).
liftW2 :: (Monad m, ReactiveValueWrite a b m, ReactiveValueWrite d e m)
       => (c -> (b,e)) -> a -> d -> ReactiveFieldWrite m c
liftW2 f e1 e2 = ReactiveFieldWrite setter
  where setter x = do let (v1,v2) = f x
                      reactiveValueWrite e1 v1
                      reactiveValueWrite e2 v2

-- | Binary writable replicator.
--
-- r1 &.& r2 = liftW2 (\x -> (x,x)) r1 r2
--
(&.&) :: (Monad m, ReactiveValueWrite a b m, ReactiveValueWrite c b m)
      => a -> c -> ReactiveFieldWrite m b
(&.&) v1 v2 = ReactiveFieldWrite $ \x -> do
  reactiveValueWrite v1 x
  reactiveValueWrite v2 x


-- | Lift a parameterised monadic transformation onto an RV.
liftMW :: (Monad m, ReactiveValueWrite a b m)
       => (c -> m b) -> a -> ReactiveFieldWrite m c
liftMW f e = ReactiveFieldWrite setter
  where setter x = reactiveValueWrite e =<< f x

-- | Make a RW RV read only
readOnly :: ReactiveValueRead r a m => r -> ReactiveFieldRead m a
readOnly r = ReactiveFieldRead (reactiveValueRead r) (reactiveValueOnCanRead r)

-- | Make a RW RV write only
writeOnly :: ReactiveValueWrite r a m => r -> ReactiveFieldWrite m a
writeOnly r = ReactiveFieldWrite (reactiveValueWrite r)

-- ** Lift monadic actions/sinks (setters) and sources (getters)

-- *** Lifting (sink) computations into writable RVs.

-- | Wrap a monadic computation in a writable reactive value.
wrapMW :: (a -> m ()) -> ReactiveFieldWrite m a
wrapMW = ReactiveFieldWrite

-- | Wrap a monadic computation in a writable reactive value.
-- It discards the written value and executes the operation.
--
-- Note: Because the value is discarded, the resulting RV is
-- polymorphic in the value that may be written to it. Using
-- 'wrapDo_' may save you some extra type signatures.
wrapDo :: m () -> ReactiveFieldWrite m a
wrapDo = wrapMW . const

-- | Wrap a monadic computation in a writable reactive value of type
-- unit. It discards the written value and executes the operation.
wrapDo_ :: m () -> ReactiveFieldWrite m ()
wrapDo_ = wrapDo

-- *** Lifting (source) computations into readable RVs.

{-# ANN wrapMR "HLint: ignore Eta reduce" #-}
-- | Wrap an reading operation and an notification installer in
-- a readable reactive value.
wrapMR :: m a -> (m () -> m ()) -> ReactiveFieldRead m a
wrapMR f p = ReactiveFieldRead f p

-- | Wrap an reading operation into an RV. Because there is
-- no way to detect changes, the resulting RV is passive (does
-- not push updates).
wrapMRPassive :: Monad m => m a -> ReactiveFieldRead m a
wrapMRPassive f = ReactiveFieldRead f (const (return ()))

{-# ANN eventR "HLint: ignore Eta reduce" #-}
-- | Wrap event-handler installers in RVs
eventR :: Monad m => (m () -> m ()) -> ReactiveFieldRead m ()
eventR notifInstaller = ReactiveFieldRead (return ()) notifInstaller

-- ** Lifting onto read-write values

-- *** Bijections
newtype BijectiveFunc a b = BijectiveFunc
  { unBijectiveFunc :: (a -> b, b -> a) }

bijection :: (a -> b, b -> a) -> BijectiveFunc a b
bijection = BijectiveFunc

{-# ANN direct "HLint: ignore Redundant bracket" #-}
direct :: BijectiveFunc a b -> (a -> b)
direct = fst . unBijectiveFunc

{-# ANN inverse "HLint: ignore Redundant bracket" #-}
inverse :: BijectiveFunc a b -> (b -> a)
inverse = snd . unBijectiveFunc

type Involution a = BijectiveFunc a a
involution :: (a -> a) -> Involution a
involution f = BijectiveFunc (f, f)

-- *** Actual lifting
liftRW :: (Monad m, ReactiveValueReadWrite a b m)
       => BijectiveFunc b c -> a -> ReactiveFieldReadWrite m c
liftRW (BijectiveFunc (f1, f2)) e = ReactiveFieldReadWrite setter getter notifier
  where ReactiveFieldRead getter notifier = liftR f1 e
        ReactiveFieldWrite setter         = liftW f2 e

liftRW2 :: (Monad m, ReactiveValueReadWrite a b m, ReactiveValueReadWrite c d m)
        => BijectiveFunc e (b,d) -> a -> c -> ReactiveFieldReadWrite m e
liftRW2 (BijectiveFunc (f1, f2)) e1 e2 = ReactiveFieldReadWrite setter getter notifier
  where ReactiveFieldRead getter notifier = liftR2 (curry f2) e1 e2
        ReactiveFieldWrite setter         = liftW2 f1 e1 e2

pairRW :: (Monad m,
           ReactiveValueReadWrite a b m,
           ReactiveValueReadWrite c d m)
       => a -> c
       -> ReactiveFieldReadWrite m (b, d)
pairRW = liftRW2 (bijection (id, id))

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

-- * Merging

{-# ANN lMerge "HLint: ignore Use const" #-}
-- | Left merge (give priority to the value on the left)
lMerge :: (Monad m, ReactiveValueRead a v m, ReactiveValueRead b v m)
       => a -> b -> ReactiveFieldRead m v
lMerge = liftR2 (\a _ -> a)

-- | Right merge (give priority to the value on the left)
rMerge :: (Monad m, ReactiveValueRead a v m, ReactiveValueRead b v m)
       => a -> b -> ReactiveFieldRead m v
rMerge = liftR2 (\_ b -> b)

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

-- | A form of binary readable lifting that passifies the second RV but reads
-- exclusively from it.
--
-- governingR r1 r2 = rMerge r1 (passively r2)

governingR :: (ReactiveValueRead a b m,  ReactiveValueRead c d m)
           => a -> c -> ReactiveFieldRead m d
governingR r c = ReactiveFieldRead getter notifier
  where getter   = reactiveValueRead c
        notifier = reactiveValueOnCanRead r

-- * Conditionals

-- Check condition and notify only when holds
ifRW_ :: (Monad m, ReactiveValueRead c Bool m, ReactiveValueReadWrite v a m)
      => c -> v
      -> ReactiveFieldReadWrite m a
ifRW_ c r = ReactiveFieldReadWrite setter getter notifier
  where setter = reactiveValueWrite r
        getter = reactiveValueRead r
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

-- Check condition and notify only when holds
guardRO :: (Monad m, ReactiveValueRead c Bool m)
        => c
        -> ReactiveFieldRead m Bool
guardRO c = ReactiveFieldRead getter notifier
  where getter   = reactiveValueRead c
        -- If either changes, the value *may* be propagated
        notifier = reactiveValueOnCanRead c . when'

        -- Propagate only if the condition holds
         where when' m = do x <- reactiveValueRead c
                            when x m

-- Check condition and notify only when holds
guardRO' :: (Monad m, ReactiveValueRead c a m)
         => c
         -> (a -> Bool)
         -> ReactiveFieldRead m a
guardRO' c p = ReactiveFieldRead getter notifier
  where getter   = reactiveValueRead c
        -- If either changes, the value *may* be propagated
        notifier = reactiveValueOnCanRead c . when'

        -- Propagate only if the condition holds
         where when' m = do x <- reactiveValueRead c
                            when (p x) m

-- Category theoretic definitions

-- Functor definitions
instance (Functor m, Monad m) => Functor (ReactiveFieldRead m) where
  fmap = liftR

-- FIXME: I might not want to provide this: the contravariant library
-- depends on transformers.
-- (ReactiveFieldRead getter notifier) = ReactiveFieldRead (fmap f getter) notifier
instance (Monad m) => Contravariant (ReactiveFieldWrite m) where
  contramap = liftW

instance Monad m => GFunctor (ReactiveFieldReadWrite m) BijectiveFunc where
  gmap = liftRW
