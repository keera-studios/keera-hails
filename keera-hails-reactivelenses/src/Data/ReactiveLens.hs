{-# LANGUAGE Rank2Types #-}
-- | Apply lenses onto Reactive Values, produced RVs that focus
-- only on a part of the value.
module Data.ReactiveLens where

import Control.Lens
import Data.ReactiveValue

-- | Apply a lens to a reactive value, produced another reactive value that
-- focuses on a part of the first.
reactiveFromLens :: (Monad m, Functor m)
                 => ReactiveFieldReadWrite m a
                 -> Lens' a b
                 -> ReactiveFieldReadWrite m b
reactiveFromLens rv@(ReactiveFieldReadWrite _setter getter notifier) l =
    ReactiveFieldReadWrite setter' getter' notifier
  where setter' v = reactiveValueModify rv (set l v)
        getter'   = fmap (view l) getter

-- I'd love to write this, but it's not possible because Haskell does not
-- allow partially applied type synonyms anywhere.
-- instance Monad m => GFunctor (ReactiveFieldReadWrite m) Lens' where
--   gmap = reactiveFromLens

-- | An infix version of 'reactiveFromLense'.
--
-- DUE to a strange problem with GHC-7.10, I cannot use the following
-- equivalente definition: 'flip' 'reactiveFromLens'.
(<$$$>) :: (Monad m, Functor m)
        => Lens' a b -> ReactiveFieldReadWrite m a -> ReactiveFieldReadWrite m b
(<$$$>) l rv@(ReactiveFieldReadWrite _setter getter notifier) =
    ReactiveFieldReadWrite setter' getter' notifier
  where setter' v = reactiveValueModify rv (set l v)
        getter'   = fmap (view l) getter

-- I think this bit would need monadic lenses
-- reactiveLens :: Lens' a b -> Lens' (ReactiveFieldReadWrite m a) (ReactiveFieldReadWrite m b)
-- reactiveLens = undefined
