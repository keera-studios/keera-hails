-- | The environment that contains both the view and the model.
--
module Hails.MVC.View.GladeView where

-- Internal libraries
import Graphics.UI.Gtk.Builder

class GladeView a where
   ui :: a -> Builder
