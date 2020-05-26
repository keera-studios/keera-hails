{-# LANGUAGE TemplateHaskell #-}

-- External imports
import Control.Monad  (forM_)
import Data.FileEmbed (embedStringFile)
import Data.String    (IsString)

-- External imports: GHCJS
import Data.Maybe                     (fromJust)
import GHCJS.DOM                      (currentDocumentUnchecked)
import GHCJS.DOM.Document             (getBodyUnchecked, getHeadUnchecked)
import GHCJS.DOM.Element              (setInnerHTML)
import GHCJS.DOM.NonElementParentNode (getElementById)
import GHCJS.DOM.Types                (Element (..), HTMLButtonElement (..),
                                       HTMLInputElement (..), uncheckedCastTo)

-- External imports: Keera Hails project
import Data.CBMVar          (newCBMVar)
import Data.CBMVar.Reactive (cbmvarReactiveRW)
import Data.ReactiveValue   (modRW, (<:=), (<^>), (=:>))
import Hails.MVC.View.HTML  (htmlElementClick, inputTextReactive)

main :: IO ()
main = do
  doc <- currentDocumentUnchecked
  header <- getHeadUnchecked doc
  setInnerHTML header (staticHeader :: String)

  body <- getBodyUnchecked doc
  setInnerHTML body (staticBody :: String)

  -- Create reactive RV that propagates the text of the text field
  inputField <- uncheckedCastTo HTMLInputElement <$> fromJust
                                                 <$> getElementById doc ("myinput" :: String)

  let inputFieldText = inputTextReactive inputField

  -- Create reactive RV that returns the value x when the button is clicked
  let constReactiveButton xid xval = do
        button <- uncheckedCastTo HTMLButtonElement <$> fromJust
                                                    <$> getElementById doc (xid :: String)

        return $ const xval <^> htmlElementClick button

  -- Add all number and operator buttons
  nums <- mapM (uncurry constReactiveButton)
               [ ("num" ++ show x, x) | x <- [1..4] ]

  -- Initialize model
  model <- cbmvarReactiveRW <$> newCBMVar 0

  -- Connect model and text box
  inputFieldText <:= (show <^> model)

  forM_ nums $ \button ->
    button =:> model

-- | Static partial loaded from file
staticHeader :: IsString a => a
staticHeader = $(embedStringFile "data/head.html")

-- | Static partial loaded from file.
staticBody :: IsString a => a
staticBody = $(embedStringFile "data/body.html")
