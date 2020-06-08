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

  -- Add all number
  nums <- mapM (uncurry constReactiveButton)
               [ ("num" ++ show x, x) | x <- [0..9] ]

  -- Add all operator buttons
  operators <- mapM (uncurry constReactiveButton)
                 [ ("opadd", (+)), ("opsub", (-))
                 , ("opmul", (*)), ("opdiv", safeDiv)
                 ]

  -- Add all action buttons
  actions <- mapM (uncurry constReactiveButton)
               [ ("acteq", Equals), ("actclr", Clear) ]

  -- Initialize model
  model <- cbmvarReactiveRW <$> newCBMVar mkCalculator

  -- Connect model and text box
  inputFieldText <:= ((show . currentValue) <^> model)

  forM_ nums $ \button ->
    button =:> modRW addDigit model

  forM_ operators $ \button ->
    button =:> modRW applyOperator model

  forM_ actions $ \button ->
    button =:> modRW applyAction model

-- | Static partial loaded from file
staticHeader :: IsString a => a
staticHeader = $(embedStringFile "data/head.html")

-- | Static partial loaded from file.
staticBody :: IsString a => a
staticBody = $(embedStringFile "data/body.html")

-- * Auxiliary numeric operations

-- | Divide two integer numbers, returning the first if the second one is
-- zero.
safeDiv :: Int -> Int -> Int
safeDiv x 0 = x
safeDiv x y = x `div` y

-- | Operator button in a calculator. We support both binary number
-- operators and other operatots that whould return the final value.
data Action = Equals | Clear

data Calculator = Calculator

mkCalculator :: Calculator
mkCalculator = Calculator

currentValue :: Calculator -> Int
currentValue _ = 0

addDigit :: Calculator -> Int -> Calculator
addDigit calc _ = calc

applyOperator :: Calculator -> (Int -> Int -> Int) -> Calculator
applyOperator calc _ = calc

applyAction :: Calculator -> Action -> Calculator
applyAction calc _ = calc
