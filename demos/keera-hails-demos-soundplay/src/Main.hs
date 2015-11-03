import           Control.Concurrent
import           Control.Monad
import           Data.ReactiveValue
import           Foreign.ForeignPtr
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Reactive
import           Graphics.UI.Gtk.Reactive.Gtk2
import           Graphics.UI.SDL                as SDL
import qualified Graphics.UI.SDL.Mixer.General  as SDL.Mixer
import qualified Graphics.UI.SDL.Mixer.Channels as SDL.Mixer.Channels
import qualified Graphics.UI.SDL.Mixer.Types    as SDL.Mixer.Types
import qualified Graphics.UI.SDL.Mixer.Samples  as SDL.Mixer.Samples

import           Paths_keera_hails_demos_soundplay

main :: IO ()
main = do
  -- SDL Stuff (not reactive at all)
  SDL.init [InitAudio]
  SDL.Mixer.openAudio 44100 SDL.Mixer.AudioS16LSB 2 4096
  SDL.Mixer.Channels.allocateChannels 16
  SDL.Mixer.Channels.volume (-1) 50

  -- View Stuff (Gtk, not reactive at all)
  initGUI
  window <- windowNew
  set window [windowTitle := "Text Entry", containerBorderWidth := 10]

  vb <- vBoxNew False 0
  containerAdd window vb

  btn <- buttonNewWithLabel "Press me"
  boxPackStart vb btn PackNatural 0

  adj   <- adjustmentNew 50 0 100 1 10 0
  scale <- hScaleNew adj
  boxPackStart vb scale PackNatural 0

  widgetShowAll window

  -- Controller (this is the reactive stuff)

  -- Adjustment RV (see Gtk Scales). Combination of two reactive values:
  -- The first changes when the slider moves, but has no value.
  -- The second has the slider value, but does not propagate changes.
  -- governingRW makes the change-propagating RV control the value-carrying RV.
  let adjValue = onValueChangedReactive adj `governingRW`
                 adjustmentValuePassive adj

  -- SDL volume Passive RV
  let sdlVolume = ( SDL.Mixer.Channels.volume (-1) (-1)
                  , void . SDL.Mixer.Channels.volume (-1)
                  )

  -- Controller Rules
  buttonActivatedReactive btn  =:> (playSDLAsync <=< getDataFileName) "baby.wav"
  (round <^> adjValue)         =:> sdlVolume -- This also works: (SDL.Mixer.Channels.volume (-1))
  objectDestroyReactive window =:> mainQuit

  -- Run!
  mainGUI

-- IO SDL code to play sound asynchronously. No special reactive stuff here.
playSDLAsync :: FilePath -> IO ()
playSDLAsync fp = void $ forkIO $
  wave <- SDL.Mixer.Samples.loadWAV fp
  playChannelWhole (-1) wave 0

playChannelWhole :: Channel -> Chunk -> Int -> IO ()
playChannelWhole channel wave times = do
  c <- SDL.Mixer.Channels.playChannel channel wave times
  delayTillNotPlaying c
  touchForeignPtr wave

delayTillNotPlaying :: Int -> IO ()
delayTillNotPlaying c = go
 where go = do
   p <- SDL.Mixer.Channels.isChannelPlaying c
   if p
     then putStrLn "Step" >> threadDelay 1000000 >> go
     else putStrLn "Done"
