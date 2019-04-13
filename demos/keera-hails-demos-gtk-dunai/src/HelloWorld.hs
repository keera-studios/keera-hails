import Control.Monad.Trans.MSF.Reader
import Data.IORef
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive
import Graphics.UI.Gtk.Reactive.Gtk2

main = do
   -- View
   initGUI
   window <- windowNew
   set window [windowTitle := "Text Entry", containerBorderWidth := 10]

   -- RV sink that executes one step of an MSF
   dunaiRV <- dunaiWO (arr (\x -> (x,())) >>> runReaderS myMSF >>> arrM print)

   vb <- vBoxNew False 0
   containerAdd window vb

   txtfield <- entryNew
   boxPackStart vb txtfield PackNatural 0

   lbl <- labelNew (Nothing :: Maybe String)
   boxPackStart vb lbl PackNatural 0

   widgetShowAll window

   -- Controller Rules
   (printMsg <^> entryTextReactive txtfield) =:> labelTextReactive lbl
   objectDestroyReactive window              =:> mainQuit
   (entryTextReactive txtfield) =:> dunaiRV

   -- Run!
   mainGUI

myMSF :: Monad m => MSF (ReaderT String m) () Int
myMSF = constM ask >>> arr length

-- Pure controller functions that can be debugged independently
printMsg ""  = ""
printMsg txt = "\"" ++ txt ++ "\" is " ++ qual ++ " to its reverse"
 where qual | txt == reverse txt = "equal"
            | otherwise          = "not equal"

dunaiWO :: MSF IO a () -> IO (ReactiveFieldWrite IO a)
dunaiWO msf = do
  msfRef <- newIORef msf
  return $ wrapMW $ \x -> do
             msf' <- readIORef msfRef
             ((), msf'') <- unMSF msf' x
             writeIORef msfRef msf''
