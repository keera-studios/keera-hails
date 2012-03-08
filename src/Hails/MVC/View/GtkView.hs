-- | Contains basic operations related to the GUI
module Hails.MVC.View.GtkView where

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