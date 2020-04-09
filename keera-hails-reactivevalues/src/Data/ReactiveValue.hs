{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
--
-- /Reactive Values/ (RVs) are typed mutable variables with a change
-- notification mechanism. They are defined by providing a way to /read/ the
-- value, a way to /change/ it, and a way to install an /react/ when the value
-- has changed.
--
-- RVs are abstract (i.e., a type class). This module defines a class and
-- manipulation operations, but you need an actual instance of RV underneath.
-- For a way to turn 'IORef's into RVs, see the example below. GUI toolkits can
-- use existing event-handling mechanisms to make widget attributes Reactive
-- Values without additional boilerplate. A number of
-- <https://github.com/keera-studios/keera-hails backends> (GUIs, devices,
-- files, network, FRP) are supported.
--
-- RVs are complemented with /Reactive Relations/, which connect RVs and keep
-- them /in sync/ during execution.
--
-- A very simple example of an RV is the following construction, in
-- which passive IORefs are turned into active Reactive Values.
--
-- @
-- import Control.Concurrent (threadDelay)
-- import Control.Monad      (forever)
--
-- -- From the package keera-callbacks
-- import Data.CBRef         (installCallbackCBRef, newCBRef, readCBRef,
--                            writeCBRef)
--
-- -- From keera-hails-reactivevalues
-- import Data.ReactiveValue (ReactiveFieldReadWrite (..), ReactiveFieldWrite,
--                            reactiveValueModify, wrapMW, (=:>))
--
-- main :: IO ()
-- main = do
--
--   -- Empower IORef with callback installation mechanism
--   --
--   -- passiveCBRef :: CBRRef Integer
--   passiveCBRef <- newCBRef 0
--
--   -- Turn IORef into active reactive value (RV).
--   --
--   -- We use the type of Reactive Fields, which have a trivial RV implementation.
--   let activeCBRefRV :: ReactiveFieldReadWrite IO Integer
--       activeCBRefRV = ReactiveFieldReadWrite
--                         (writeCBRef           passiveCBRef)
--                         (readCBRef            passiveCBRef)
--                         (installCallbackCBRef passiveCBRef)
--
--   -- Define a write-only RV that prints whatever you put in it.
--   let printer :: Show a => ReactiveFieldWrite IO a
--       printer = wrapMW print
--
--   -- Connect them using a reactive rule. In a GUI application, this code would
--   -- in the controller, and would define connections between the model and
--   -- the view.
--   --
--   -- For bi-directional connections, see (=:=).
--   activeCBRefRV =:> printer
--
--   -- To demonstrate the connection, just loop forever and increment the
--   -- first reactive value. The change will propagate through the channel
--   -- and be printed on the screen every second.
--   forever $ do
--     threadDelay 1000000 -- 1 second
--     reactiveValueModify activeCBRefRV (+1)
-- @
--
-- For further explanations on reactive values, see the
-- <http://dl.acm.org/citation.cfm?id=2804316 Haskell Symposium paper> and
-- <https://github.com/keera-studios/keera-hails/tree/develop/demos the demos>
-- in our repository.

module Data.ReactiveValue
  ( -- * Reactive Values
    -- $rvs

    -- ** Readable Reactive Values
    -- $readablervs
    ReactiveValueRead(..)

    -- ** Writable Reactive Values

    -- $writablervs
  , ReactiveValueWrite(..)

    -- ** Read-Write Reactive Values

    -- $readwritervs
  , ReactiveValueReadWrite

    -- * Reactive Relations or Rules

    -- $rules
  , (=:>)
  , (=:=)
  , (<:=)


    -- * Reactive Fields (pure RVs)

    -- $fields
  , ReactiveFieldRead(..)
  , ReactiveFieldWrite(..)
  , ReactiveFieldReadWrite(..)

    -- $settersgetters
  , FieldGetter
  , FieldSetter
  , FieldNotifier

    -- * RV creation and manipulation

    -- ** Readable RVs

    -- $readablecombinators
  , constR
  , initRW
  , liftR
  , (<^>)
  , liftR2
  , liftR3
  , liftMR
  , readOnly
  , wrapMR
  , wrapMRPassive
  , eventR
  , lMerge
  , rMerge

    -- ** Writable RVs

    -- $writablecombinators
  , constW
  , liftW
  , liftW2
  , (&.&)
  , liftMW
  , writeOnly
  , wrapMW
  , wrapDo
  , wrapDo_


    -- ** Read-write RVs

    -- $readwritecombinators
  , liftRW
  , liftRW2
  , pairRW
  , modRW

    -- **** Bijective functions
  , BijectiveFunc
  , bijection
  , direct
  , inverse
  , Involution
  , involution

    -- **** Low-level operations
  , reactiveValueModify


    -- * Controlling change

    -- $changecontrol

    -- ** Stopping change propagation
  , eqCheck
  , passivelyR
  , passivelyRW

    -- ** Governing
  , governingR
  , governingRW

    -- ** Guarding
  , ifRW
  , ifRW_
  , guardRO
  , guardRO'

    -- * Activatable RVs

    -- $activatable
  , ReactiveValueActivatable(..)
  , ReactiveFieldActivatable
  , mkActivatable
  )
 where

import Control.Monad
import Control.GFunctor -- Functors parameterised over the morphisms
                        -- in the source category
import Data.Functor.Contravariant

-- $rvs
--
-- Reactive Values are an abstraction over values that change over the execution
-- of a program, and whose change we can be aware of.
--
-- There is no unique, canonical implementation of RVs: they are defined as
-- a collection of type classes, and you are free to make your existing
-- framework reactive by providing the necessary instances.
--
-- RVs are distinguished by the API they offer, mainly whether it is possible
-- to read them, to write to them, or both. A /readable/ RV is one that whose
-- value we can read (whether it is read-only or read-write, or whether it will
-- actively propagate changes to it or not, is a different matter). Analogously,
-- a /writable/ RV is one that we can write to (write-only or read-write).
--
-- We also distinguish between /active/ RVs (i.e., those that actively propagate
-- changes through the Reactive Relations they are connected to) and /passive/
-- RVs (those that do not propagate changes). It is possible to "silence" an
-- RV by minimizing unnecesssary change, or attaching it to another RV that
-- determines when change propagates (see governing and guarding below).

-- $readablervs
--
-- Readable reactive values are mutable values that can be read and, possibly,
-- trigger notifications when they change.
--
-- RVs without event handling are considered /passive/. That is their default
-- behaviour if 'reactiveValueOnCanRead' is not specialised.
-- BEWARE: Active and passive RVs are not differentiated at the type level.
--
-- You are responsible of installing any potential thread-safety
-- mechanisms when you implement instances, and to ensure that operations
-- are executed in the right thread (some GUI toolkits may require that).
-- It is important that the way that ensured that monadic actions are
-- executed in the right thread can be nested; otherwise, some propagation can
-- block.

-- | A class for all Readable RVs. They can be read-only or read-write.
--
-- If your type is also writeable (a writable RV or simply if it is not
-- constant), you can include a change handler installer here. By default
-- no change handlers are installed.
--
-- Use a monad with "error" if reading or installing a handler can fail.
class Monad m => ReactiveValueRead a b m | a -> b, a -> m where

  -- | Install a handler that will be executed when the reactive value
  -- changes.
  reactiveValueOnCanRead :: a -> m () -> m ()
  reactiveValueOnCanRead _ _ = return ()

  -- | Provide the last known value for this reactive value.
  reactiveValueRead :: a -> m b

  {-# MINIMAL reactiveValueRead #-}

-- | Monadic actions are readable, but they do not provide any
-- change notification.
instance ReactiveValueRead (IO a) a IO where
  -- | Executes the monadic action and provides a value.
  reactiveValueRead = id

-- | Pairs carrying a monadic action as their first component are Readable RVs.
instance (Functor m, Monad m) => ReactiveValueRead (m a, a -> m b) a m where
  reactiveValueRead = fst

-- $writablervs
--
-- Writable reactive values are those that we can write to.
-- They behave like sinks: there are no guarantees that anything happens,
-- or result codes.
--
-- You are responsible of installing any potential thread-safety
-- mechanisms when you implement instances, and to ensure that operations
-- are executed in the right thread (some GUI toolkits may require that).
-- It is important that the way that ensured that monadic actions are
-- executed in the right thread can be nested; otherwise, some propagation can
-- block.

-- | A minimal type class for all mutable values. Use a monad with error
-- if changing the value can fail.
class ReactiveValueWrite a b m | a -> b, a -> m where
  reactiveValueWrite :: a -> b -> m ()

-- | Monadic actions are trivially writable RVs in that monad, by ignoring the
-- argument and executing the action. This is particularly suitable for IO code
-- that just runs an action when a given value changes.
instance ReactiveValueWrite (IO a) () IO where
  reactiveValueWrite m _ = void m

-- | Monadic actions parameterised over an input are trivially writable RVs in
-- that monad. This is particularly suitable for IO code that just runs an
-- action when a given value changes, using that value.
instance (Functor m, Monad m) => ReactiveValueWrite (a -> m b) a m where
  reactiveValueWrite f v = void (f v)

-- | To facilitate creating RW reactive values from monadic actions, pairs
-- of a getter and a setter are also RVs.
instance ReactiveValueWrite (a -> m b) a m => ReactiveValueWrite (m a, a -> m b) a m where
  reactiveValueWrite (_, f) = reactiveValueWrite f

-- $readwritervs
--
-- RVs can be readable and writable, which is useful to create bi-directional
-- rules (combinators like '(=:=)' require this instance).
--

-- | Read-write Reactive Values are trivially defined. This class only captures
-- the constraints of both the other classes. There is no need to implement
-- any methods.
class (ReactiveValueRead a b m, ReactiveValueWrite a b m) => ReactiveValueReadWrite a b m

-- | Pairs of a monadic action and a parametric monadic action are also RVs
instance (Functor m, Monad m) => ReactiveValueReadWrite (m a, a -> m b) a m

-- $activatable
--
-- Activatable RVs are values that never hold any data, but whose change (or
-- activation, or some sort of internal event) we need to be aware of).

-- | A class for things with a trivial field that carries unit. Buttons
-- (in any GUI library), for instance, could be a member of this class.
class ReactiveValueActivatable m a where
   defaultActivation :: a -> ReactiveFieldActivatable m

-- $rules
--
-- Reactive Rules are data dependency (data-passing) building combinators.
-- By executing them, you install the right event handlers on the right RVs, so
-- that values pass to the other RV.
--
-- Reactive Relations cannot be independently removed. If the event-dispatching
-- is handled internally by RVs, and two connected RVs are removed, then the
-- rules should also disappear with them.

-- Priorities so that we can write them infix without parenthesising
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

-- $fields
-- This is a specific implementation of RVs that does not have a custom event queue.
--
-- It can be used to return RVs in the combinators, by relying on the underlying
-- change detection and event notification system (underlying meaning or the RV
-- that these were created from).

-- | A Read-Only RV.
data ReactiveFieldRead m a =
  ReactiveFieldRead (FieldGetter m a) (FieldNotifier m a)

-- | A Write-Only RV.
newtype ReactiveFieldWrite m a =
  ReactiveFieldWrite (FieldSetter m a)

-- | A Read-Write RV.
data ReactiveFieldReadWrite m a =
  ReactiveFieldReadWrite (FieldSetter m a) (FieldGetter m a) (FieldNotifier m a)

-- | A trivial type for Readable RVs that carry unit. They can be used for
-- buttons, or events without data.
type ReactiveFieldActivatable m = ReactiveFieldRead m ()

instance Monad m => ReactiveValueRead (ReactiveFieldRead m a) a m where
  reactiveValueOnCanRead (ReactiveFieldRead _ notifier) = notifier
  reactiveValueRead (ReactiveFieldRead getter _)        = getter

instance Monad m => ReactiveValueWrite (ReactiveFieldWrite m a) a m where
  reactiveValueWrite (ReactiveFieldWrite setter) = setter

instance Monad m => ReactiveValueRead (ReactiveFieldReadWrite m a) a m where
  reactiveValueOnCanRead (ReactiveFieldReadWrite _ _ notifier) = notifier
  reactiveValueRead (ReactiveFieldReadWrite _ getter _)        = getter

instance Monad m => ReactiveValueWrite (ReactiveFieldReadWrite m a) a m where
  reactiveValueWrite (ReactiveFieldReadWrite setter _ _) = setter

instance Monad m => ReactiveValueReadWrite (ReactiveFieldReadWrite m a) a m

-- $settersgetters
--
-- These are used internally for combinators that need to return RV instances. They can
-- also be used to write new backends and library extensions, but they are not
-- recommended to enclose application models. For that purpose, see light models and
-- protected models instead.

-- | The type of a monadic value producer (a getter, a source).
type FieldGetter m a   = m a

-- | The type of a monadic value consumer (a setter, a sink, a slot).
type FieldSetter m a   = a -> m ()

-- | The type of an event handler installer
type FieldNotifier m a = m () -> m () -- FIXME: why does fieldnotifier have an argument

-- | Create an activatable RV from a handler installer.
mkActivatable :: Monad m => (m () -> m ()) -> ReactiveFieldActivatable m
mkActivatable f = ReactiveFieldRead getter notifier
 where getter   = return ()
       notifier = f

-- $readablecombinators

-- | A trivial RV builder with a constant value. We need this because
-- we cannot have overlapping instances with a default case, and because
-- the interpretation of lifting with RVs could be very confusing unless
-- values are lifted into RVs explicitly.
constR :: Monad m => a ->  ReactiveFieldRead m a
constR e = ReactiveFieldRead getter notifier
 where notifier _ = return ()
       getter     = return e

-- | TODO: Bad name. Should be eliminated or extended with a setter.
initRW :: Monad m => a ->  ReactiveFieldRead m a
initRW e = ReactiveFieldRead getter notifier
 where notifier _ = return ()
       getter     = return e

{-# ANN liftR "HLint: ignore Use fmap" #-}
-- | Lift a transformation onto a RV. Note that this creates a new
-- RV, it does not modify the existing RV.
liftR :: (Monad m, ReactiveValueRead a b m)
      => (b -> c)
      -> a
      -> ReactiveFieldRead m c
liftR f e = ReactiveFieldRead getter notifier
  where
    notifier = reactiveValueOnCanRead e
    getter   = liftM f (reactiveValueRead e)

-- | Shorter name for 'liftR'
(<^>) :: (Monad m, ReactiveValueRead a b m) => (b -> c) -> a -> ReactiveFieldRead m c
(<^>) = liftR

-- | Lift a transformation onto two RVs. Note that this creates a new
-- RV, it does not modify the existing RVs. When either RV changes,
-- the new one triggers a change.
liftR2 :: (Monad m, ReactiveValueRead a b m, ReactiveValueRead c d m)
       => (b -> d -> e)
       -> a
       -> c
       -> ReactiveFieldRead m e
liftR2 f e1 e2 = ReactiveFieldRead getter notifier
  where
    getter = do
      v1 <- reactiveValueRead e1
      v2 <- reactiveValueRead e2
      return (f v1 v2)

    notifier p = do
      reactiveValueOnCanRead e1 p
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

-- | Make a RW RV read only
readOnly :: ReactiveValueRead r a m => r -> ReactiveFieldRead m a
readOnly r = ReactiveFieldRead (reactiveValueRead r) (reactiveValueOnCanRead r)

-- $writablecombinators

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

-- | Make a RW RV write only
writeOnly :: ReactiveValueWrite r a m => r -> ReactiveFieldWrite m a
writeOnly r = ReactiveFieldWrite (reactiveValueWrite r)

-- $readwritecombinators

-- | Wrap a monadic computation in a writable reactive value.
wrapMW :: (a -> m ()) -> ReactiveFieldWrite m a
wrapMW = ReactiveFieldWrite

-- | Wrap a monadic computation in a writable reactive value.
-- It discards the written value and executes the operation.
--
-- Note: Because the value is discarded, the resulting RV is
-- polymorphic in the value that may be written to it. Using
-- 'wrapDo_' may save you some extra type signatures.
--
-- NOTE: this should be unnecessary since the introduction
-- of a default 'ReactiveValueWrite' instance for monadic
-- actions.
wrapDo :: m () -> ReactiveFieldWrite m a
wrapDo = wrapMW . const

-- | Wrap a monadic computation in a writable reactive value of type
-- unit. It discards the written value and executes the operation.
wrapDo_ :: m () -> ReactiveFieldWrite m ()
wrapDo_ = wrapDo

-- Lifting onto read-write values

-- | Bijections
newtype BijectiveFunc a b = BijectiveFunc
  { unBijectiveFunc :: (a -> b, b -> a) }

-- | Create a bijection ('BijectiveFunc') from a couple of functions
bijection :: (a -> b, b -> a) -> BijectiveFunc a b
bijection = BijectiveFunc

{-# ANN direct "HLint: ignore Redundant bracket" #-}
-- | Obtain the direct function from a bijection
direct :: BijectiveFunc a b -> (a -> b)
direct = fst . unBijectiveFunc

{-# ANN inverse "HLint: ignore Redundant bracket" #-}
-- | Obtain the inverse function from a bijection
inverse :: BijectiveFunc a b -> (b -> a)
inverse = snd . unBijectiveFunc

-- | Involutions (functions that are the same as their inverse)
type Involution a = BijectiveFunc a a

-- | Create an involution from a function
involution :: (a -> a) -> Involution a
involution f = BijectiveFunc (f, f)

-- | Lift a bijection onto a read-write RV
liftRW :: (Monad m, ReactiveValueReadWrite a b m)
       => BijectiveFunc b c -> a -> ReactiveFieldReadWrite m c
liftRW (BijectiveFunc (f1, f2)) e = ReactiveFieldReadWrite setter getter notifier
  where ReactiveFieldRead getter notifier = liftR f1 e
        ReactiveFieldWrite setter         = liftW f2 e

-- | Lift a bijection onto two read-write RVs
liftRW2 :: (Monad m, ReactiveValueReadWrite a b m, ReactiveValueReadWrite c d m)
        => BijectiveFunc e (b,d) -> a -> c -> ReactiveFieldReadWrite m e
liftRW2 (BijectiveFunc (f1, f2)) e1 e2 = ReactiveFieldReadWrite setter getter notifier
  where ReactiveFieldRead getter notifier = liftR2 (curry f2) e1 e2
        ReactiveFieldWrite setter         = liftW2 f1 e1 e2

-- | Pair two read-write RVs
pairRW :: (Monad m,
           ReactiveValueReadWrite a b m,
           ReactiveValueReadWrite c d m)
       => a -> c
       -> ReactiveFieldReadWrite m (b, d)
pairRW = liftRW2 (bijection (id, id))

-- | Add an equality check to the setter of a Read-Write RV, effectively stopping
-- all unnecessary change (the RV is not modified if it has not changed).
{-# INLINE eqCheck #-}
eqCheck :: (Eq v, Monad m) => ReactiveFieldReadWrite m v -> ReactiveFieldReadWrite m v
eqCheck (ReactiveFieldReadWrite setter getter notifier) = ReactiveFieldReadWrite setter' getter notifier
 where setter' v = do o <- getter
                      when (o /= v) $ setter v


-- | Lift a function that takes an old value and a new input and creates a new
-- value. It is similar to how fold works: the RV represents the accumulator,
-- and the values are provided in succession by writing to the resulting RV.
modRW :: (Monad m, ReactiveValueReadWrite a b m)
      => (b -> c -> b) -> a -> ReactiveFieldWrite m c
modRW f rv = ReactiveFieldWrite setter
  where setter c = do b <- reactiveValueRead rv
                      let b' = f b c
                      reactiveValueWrite rv b'

-- | Apply a modification to an RV. This modification is not attached to
-- the RV, and there are no guarantees that it will be atomic (if you need
-- atomicity, check out STM).
reactiveValueModify :: (Monad m, ReactiveValueReadWrite a b m) => a -> (b -> b) -> m ()
reactiveValueModify r f = reactiveValueWrite r . f =<< reactiveValueRead r

{-# ANN lMerge "HLint: ignore Use const" #-}
-- | Left merge (give priority to the value on the left)
lMerge :: (Monad m, ReactiveValueRead a v m, ReactiveValueRead b v m)
       => a -> b -> ReactiveFieldRead m v
lMerge = liftR2 (\a _ -> a)

-- | Right merge (give priority to the value on the right)
rMerge :: (Monad m, ReactiveValueRead a v m, ReactiveValueRead b v m)
       => a -> b -> ReactiveFieldRead m v
rMerge = liftR2 (\_ b -> b)

-- $changecontrol
--
-- Sometimes you need to create complex liftings between RVs in which
-- only changes to one of them should provoke change propagation.
-- These combinators allow you to stop propagation (making RVs passive),
-- make one RV control the change propagation of another (governance),
-- filter propagation based on some condition (guards) and have a
-- boolean-carrying RV guard another.

-- Turning an active RV into a passive one (does not propagate changes)
-- Note that this does not really affect the RV itself, only produces a new
-- RV that will not propagate changes. So, if used in a reactive relation,
-- values will not get propagated when they change. It is useful in combination
-- with lifts, to achieve things similar to Yampa's tagging, but this might
-- be more general.

-- | Create a passive RO RV backed by another Readable RV by disabling change
-- propagation.
passivelyR :: (Monad m, ReactiveValueRead a b m)
           => a -> ReactiveFieldRead m b
passivelyR rv =
  ReactiveFieldRead (reactiveValueRead rv) (\_ -> return ())

-- | Create a passive RW RV backed by another RW RV by disabling change
-- propagation.
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

-- | A form of binary read-writable lifting that passifies the second RV but reads
-- exclusively from it.

governingRW :: (ReactiveValueRead a b m,  ReactiveValueReadWrite c d m)
           => a -> c -> ReactiveFieldReadWrite m d
governingRW r c = ReactiveFieldReadWrite setter getter notifier
  where getter   = reactiveValueRead c
        setter   = reactiveValueWrite c
        notifier = reactiveValueOnCanRead r

-- | Check condition, and write or notify only when it holds.
ifRW :: (Monad m, ReactiveValueRead c Bool m, ReactiveValueReadWrite v a m)
     => c -> v
     -> ReactiveFieldReadWrite m a
ifRW c r = ReactiveFieldReadWrite setter getter notifier
  where setter x   = do b <- reactiveValueRead c
                        when b $ reactiveValueWrite r x
        getter     = reactiveValueRead r
        -- If either changes, the value *may* be propagated
        notifier p = do reactiveValueOnCanRead c (when' p)
                        reactiveValueOnCanRead r (when' p)

        -- Propagate only if the condition holds
         where when' m = do b <- reactiveValueRead c
                            when b m

-- | Check condition and notify only when holds (but writing occurs
-- regardless).
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

-- | Check RV carrying a 'Bool', and notify only when it changes and it is
-- 'True'.
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

-- | Check RV and notify only when condition on the value holds.
--
-- (stops propagation by filtering on the new value).
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
