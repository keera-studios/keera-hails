-- | This class encapsulates GUI apis with some basic common operations:
-- initialise the GUI, destroy the GUI, execute in the GUI Thread, etc.
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk

-- NOTE: This code is stable, but the design is experimental.
-- It works fine, but I doubt it's a good solution in the long term. In
-- particular, I do not like having to use the dummy type Null to define the
-- Class.
module Hails.MVC.View where

-- | Null is a parametric datatype with a non-parametric constructor.

-- NOTE: IT simplifies the class definition.
data Null a = Null

-- | A Class for View (GUI) managers. GUI managers usually have similar
-- operations: initialise, destroy, run operation in the GUI thread, etc.
-- This class encapsulates all these operations to provide a unique interface.
class View a where
  initView    :: Null a -> IO ()
  createView  :: IO a
  startView   :: a -> IO ()
  onViewSync  :: a -> IO b -> IO b
  onViewAsync :: a -> IO () -> IO ()
  destroyView :: a -> IO ()

-- | An Element Accessor to access elements of kind
-- b from Views of kind a is a function that takes an
-- a and returns a b.
--
-- This type is defined to make signatures shorter
-- and more declarative.
type ViewElementAccessor a b = (a -> b)

-- | An Element Accessor to access elements of kind
-- b from Views of kind a is a function that takes an
-- a and returns an IO b.
--
-- This is the IO counterpart of the previous type. It is
-- used more often because most element accessors run
-- inside the IO monad.
type ViewElementAccessorIO a b = ViewElementAccessor a (IO b)
