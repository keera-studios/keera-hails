-- | The environment that contains both the view and the model.
--
-- | FIXME: In a very rails-like move, this module will likely be
-- exactly the same for all programs, so we should try to put
-- a "Convention-over-configuration" policy in place and remove this
-- unless it must be adapted by the user.
module Hails.MVC.View.GladeView where

-- Internal libraries
import Graphics.UI.Gtk.Builder

class GladeView a where
   ui :: a -> Builder
