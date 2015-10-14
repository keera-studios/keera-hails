{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.ReactiveLens where

import Control.Lens
import Data.ReactiveValue

reactiveFromLens :: (Monad m, Functor m)
                 => ReactiveFieldReadWrite m a
                 -> Lens' a b
                 -> ReactiveFieldReadWrite m b
reactiveFromLens (ReactiveFieldReadWrite setter getter notifier) l =
  ReactiveFieldReadWrite setter' getter' notifier
 where setter' v = setter.(set l v) =<< getter
       getter'   = fmap (view l) getter

-- I'd love to write this, but it's not possible because Haskell does not
-- allow partially applied type synonyms anywhere.
-- instance Monad m => GFunctor (ReactiveFieldReadWrite m) Lens' where
--   gmap = reactiveFromLens

-- | Why use such a big function name? Well, there are very good
-- reasons. One is to get back to the Lens implementors, who
-- always choose names that are incredibly easy to pronounce.
-- Another is because it's based on <$> and <$$>.
(<$$$>) :: (Monad m, Functor m)
        => Lens' a b -> ReactiveFieldReadWrite m a -> ReactiveFieldReadWrite m b
(<$$$>) = flip reactiveFromLens

-- I think this bit would need monadic lenses
-- reactiveLens :: Lens' a b -> Lens' (ReactiveFieldReadWrite m a) (ReactiveFieldReadWrite m b)
-- reactiveLens = undefined
