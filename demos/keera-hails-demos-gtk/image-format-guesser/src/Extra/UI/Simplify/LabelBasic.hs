module Extra.UI.Simplify.LabelBasic
    (installHandlers)
  where

-- External libraries
import Control.Arrow
import Control.Monad
import Graphics.UI.Gtk
import Graphics.UI.View hiding (View, onViewAsync)

-- Internal libraries
import CombinedEnvironment
import View
import Model.ProtectedModel

type Accessor a = ViewElementAccessor' View a
type Getter a   = ProtectedModel -> IO a

installHandlers :: [ ModelEvent ] -> Accessor Label -> Getter String -> CRef -> IO()
installHandlers evs lblF getter cref = do
  pm <- fmap model $ readIORef cref
  mapM_ (\ev -> onEvent pm ev (condition cref lblF getter)) evs

-- | Enforces the condition
condition :: CRef -> Accessor Label -> Getter String -> IO()
condition cref lblF getter = onViewAsync $ do
  (vw, pm) <- fmap (view &&& model) $ readIORef cref
  lbl           <- lblF vw
  curViewValue  <- get lbl labelLabel
  curModelValue <- getter pm
  when (curViewValue /= curModelValue) $
    set lbl [ labelLabel := curModelValue ]
