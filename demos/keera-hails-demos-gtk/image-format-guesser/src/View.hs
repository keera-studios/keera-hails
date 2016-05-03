-- | Contains basic operations related to the GUI
module View where

-- External libraries
import Graphics.UI.Gtk
import Graphics.UI.Gtk.GenericView (GUI(..))

-- Internal libraries
import View.MainWindow.Objects

-- | Initialises the GUI. This must be called before
-- any other GUI operation.
initView :: IO ()
initView = initGUI >>= \_ -> return ()

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

instance GUI View where
  initialise = createView

-- | This datatype should hold the elements that we must track in the future
-- (for instance, treeview models)
data View = View
  { mainWindowBuilder :: Builder
  }

createView :: IO View
createView = do
  bldr <- loadInterface

  return
    View
      { mainWindowBuilder = bldr
      }

