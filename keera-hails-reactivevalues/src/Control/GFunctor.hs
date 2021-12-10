{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
--
-- Functors parameterised over the morphisms in the source category.
module Control.GFunctor where

infixl 8 <$$>

-- class ContravariantFunctor f where
--   cmap :: (b -> a) -> f a -> f b
--

-- | A class for Functors in which the morphisms in the source category do not
-- have to be of kind arrow '(->)', but can be anything (see the parameter
-- 'm').
class GFunctor f m where
  -- | Map parameterised over the morphisms in the source category.
  gmap :: m a b -> f a -> f b

-- | Trivial instance for the arrow morphism '(->)'. Anything
-- that is a functor is also a GFunctor in the trivial way.
instance (Functor a) => GFunctor a (->) where
  gmap = fmap

-- | A more readable (ignorable) name for 'gmap'.
(<$$>) :: GFunctor f m => m a b -> f a -> f b
(<$$>) = gmap

-- class GFunctor f m => GApplicative f m where
--   gpure :: a -> f a
--   (<**>) :: f (m a b) -> f a -> f b
