-- |
-- Copyright   : (C) Keera Studios Ltd, 2020
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
--
-- Abstract view.
--
-- This module publishes an operation that defines the view in an abstract
-- way (as a collection of reactive values), and an operation to construct
-- that view. Currently, HTML is supported as a backend via GHCJS, but
-- more backends could be supported and a selection via CPP flags could
-- take place in this module.
module View
    ( UI(..)
    , buildUI
    )
  where

import View.Types (UI(..))
import View.HTML  (buildUI)
