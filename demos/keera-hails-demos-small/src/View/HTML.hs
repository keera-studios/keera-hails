{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright   : (C) Keera Studios Ltd, 2020
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
--
-- View implementation in HTML.
module View.HTML where

-- External imports
import Data.FileEmbed (embedStringFile)
import Data.Maybe     (fromJust)
import Data.String    (IsString)

-- External imports: from keera-hails
import Data.ReactiveValue  ((<^>))
import Hails.MVC.View.HTML (htmlElementClick, inputTextReactive)

-- External imports: GHCJS only
import Data.JSString                  (JSString)
import GHCJS.DOM                      (currentDocumentUnchecked)
import GHCJS.DOM.Document             (createElement, getBodyUnchecked,
                                       getHeadUnchecked)
import GHCJS.DOM.Element              (setInnerHTML)
import GHCJS.DOM.Node                 (appendChild_, setTextContent)
import GHCJS.DOM.NonElementParentNode (getElementById)
import GHCJS.DOM.Types                (Document, Element (..),
                                       HTMLButtonElement (..),
                                       HTMLInputElement (..), IsGObject, JSVal,
                                       ToJSString, uncheckedCastTo)
-- Internal imports
import Data.Action (Action(Clear, Equals))
import View.Types  (UI(..))

-- | Make a view, using HTML as a backend.
buildUI :: IO UI
buildUI = do
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

  -- Compose and return abstract UI
  return $ UI nums operators actions inputFieldText

-- | Static partial loaded from template.
staticHeader :: IsString a => a
staticHeader = $(embedStringFile "data/head.html")

-- | Static partial loaded from template.
staticBody :: IsString a => a
staticBody = $(embedStringFile "data/body.html")

-- * Auxiliary

-- | Obtain and cast an element from the document.
unsafeGetElementByIdT :: (ToJSString s, IsString s, IsGObject e)
                      => Document
                      -> (JSVal -> e)
                      -> s
                      -> IO e
unsafeGetElementByIdT doc cons i =  do
  e <- fromJust <$> getElementById doc i
  return $ uncheckedCastTo cons e

-- | Divide two integer numbers, returning the first if the second one is
-- zero.
safeDiv :: Int -> Int -> Int
safeDiv x 0 = x
safeDiv x y = x `div` y
