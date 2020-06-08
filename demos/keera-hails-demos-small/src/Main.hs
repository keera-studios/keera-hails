{-# LANGUAGE TemplateHaskell #-}

-- External imports
import Control.Monad  (forM_)
import Data.FileEmbed (embedStringFile)
import Data.String    (IsString)

-- External imports: GHCJS
import Data.Maybe                     (fromJust, fromMaybe)
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
  { calcValue           :: Int
  , calcCurrentInput    :: Maybe Int
  , calcCurrentOperator :: Maybe (Int -> Int -> Int)
  }

mkCalculator :: Calculator
mkCalculator = Calculator 0 Nothing Nothing

currentValue :: Calculator -> Int
currentValue calc = fromMaybe (calcValue calc) (calcCurrentInput calc)

addDigit :: Calculator -> Int -> Calculator
addDigit calc d = calc { calcCurrentInput = Just (currentInputValue * 10 + d) }
  where
    currentInputValue = fromMaybe 0 $ calcCurrentInput calc

applyOperator :: Calculator -> (Int -> Int -> Int) -> Calculator
applyOperator calc op = case (calcCurrentOperator calc, calcCurrentInput calc) of
  (_,        Nothing) -> calc { calcCurrentOperator = Just op }
  (Nothing,  Just y)  -> calc { calcValue           = y
                              , calcCurrentOperator = Just op
                              , calcCurrentInput    = Nothing
                              }
  (Just op', Just y)  -> calc { calcValue           = op' (currentValue calc) y
                              , calcCurrentOperator = Just op
                              , calcCurrentInput    = Nothing
                              }

applyAction :: Calculator -> Action -> Calculator
applyAction calc Equals = calc { calcValue           = y'
                               , calcCurrentOperator = Nothing
                               , calcCurrentInput    = Nothing
                               }
  where
    y' = case calcCurrentOperator calc of
           Nothing -> currentValue calc
           Just op -> op (calcValue calc)  (currentValue calc)

applyAction calc Clear = case calcCurrentInput calc of
  Nothing -> calc { calcValue = 0
                  , calcCurrentOperator = Nothing
                  }
  Just 0  -> calc { calcValue = 0
                  , calcCurrentOperator = Nothing
                  , calcCurrentInput = Nothing
                  }
  Just _  -> calc { calcCurrentInput = Just 0 }
