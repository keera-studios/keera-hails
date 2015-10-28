-----------------------------------------------------------------------------
-- |
-- Module      :  Main (Tasty)
-- Copyright   :  (C) 2015 Ivan Perez
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ivan Perez <ivan.perez@keera.co.uk>
-- Stability   :  provisional
-- Portability :  portable
--
-- Test reactive value laws using Quickcheck/HUnit/Tasty. 
--
-- See the following links for instructions and documentation:
--   https://github.com/feuerbach/tasty
--   https://ocharles.org.uk/blog/posts/2013-12-03-24-days-of-hackage-tasty.html
-----------------------------------------------------------------------------

-- Testing libraries
import Test.Tasty
import Test.Tasty.QuickCheck
-- import Test.QuickCheck
-- import Test.Tasty.HUnit

-- Tested libraries
import Control.Monad.Identity
import Data.ReactiveValue

main :: IO ()
main = defaultMain $
  testGroup "ReactiveValues"
    [ testGroup "GetSetLaws"
        [ testProperty "Getting after constant initialisation" getOnConst
        ]
    ]

-- * Reactive Value laws

-- ** Reactive Value get/set laws

-- | Check that constR returns the value put in.
getOnConst :: Int -> Bool
getOnConst = 
  \val -> let rv   = constR (val :: Int)
              val' = runIdentity (reactiveValueRead rv)
          in val == val'

-- NOTE: To check that the testing system and the integration with cabal are
-- both working fine, you can use include this property in one of the tested
-- groups; the test suite should fail.
-- falseProperty = 
--   testProperty "False" $
--     \val -> not (val == (val :: Int))
