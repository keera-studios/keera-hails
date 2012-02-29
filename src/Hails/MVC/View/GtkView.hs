module Hails.MVC.View.GtkView where
-- | Contains basic operations related to the GUI
--
-- | FIXME: In a very rails-like move, this module will likely be
-- exactly the same for all programs, so we should try to put
-- a "Convention-over-configuration" policy in place and remove this
-- unless it must be adapted by the user.
-- module DefaultView where

-- External libraries
import Control.Monad
import Graphics.UI.Gtk
-- import Graphics.UI.Gtk.GtkView (GtkGUI(..))
-- import qualified Graphics.UI.Gtk.GtkView as GtkView
-- import Language.Haskell.TH

-- | Initialises the GUI. This must be called before
-- any other GUI operation.
initView :: IO ()
initView = void initGUI

-- | Starts a thread for the view.
startView :: IO ()
startView = mainGUI

-- | Executes an operation on the view thread synchronously
onViewSync :: IO a -> IO a
onViewSync = postGUISync

-- | Executes an operation on the view thread asynchronously
onViewAsync :: IO () -> IO ()
onViewAsync = postGUIAsync

-- | Destroys the view thread
destroyView :: IO ()
destroyView = mainQuit