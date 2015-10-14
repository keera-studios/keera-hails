{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE RankNTypes           #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- Functors parameterised over the morphisms in the source category
module Control.GFunctor where

infixl 8 <$$>

-- class ContravariantFunctor f where
--   cmap :: (b -> a) -> f a -> f b
-- 

class GFunctor f m where
  gmap :: m a b -> f a -> f b
 
instance (Functor a) => GFunctor a (->) where
  gmap = fmap

(<$$>) :: GFunctor f m =>m a b -> f a -> f b
(<$$>) = gmap

-- class GFunctor f m => GApplicative f m where
--   gpure :: a -> f a
--   (<**>) :: f (m a b) -> f a -> f b
