{-# LANGUAGE CPP #-}
module Paths.CustomPaths
  (module Paths_myapp
#ifndef linux_HOST_OS
  , module Paths.CustomPaths
#endif
  )
 where

import Paths_myapp

#ifndef linux_HOST_OS
vendorKey :: String
vendorKey = ""

programKey :: String
programKey = ""
#endif
