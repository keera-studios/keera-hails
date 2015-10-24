-----------------------------------------------------------------------------
-- |
-- Module      :  Main (HaddockCoverage)
-- Copyright   :  (C) 2015 Ivan Perez
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ivan Perez <ivan.perez@keera.co.uk>
-- Stability   :  provisional
-- Portability :  portable
--
-- Copyright notice: This file borrows code 
-- https://hackage.haskell.org/package/lens-4.7/src/tests/doctests.hsc
-- which is itself licensed BSD-style as well.
--
-- Run haddock on a source tree and report if anything in any
-- module is not documented.
-----------------------------------------------------------------------------
module Main where

import Control.Monad
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Regex.Posix

main :: IO ()
main = do
  -- Find haskell modules
  -- TODO: Ideally cabal should do this (provide us with the
  -- list of modules). An alternative would be to use cabal haddock
  -- but that would need a --no-html argument or something like that.
  -- Alternatively, we could use cabal haddock with additional arguments.
  --
  -- See:
  -- https://github.com/keera-studios/haddock/commit/d5d752943c4e5c6c9ffcdde4dc136fcee967c495
  -- https://github.com/haskell/haddock/issues/309#issuecomment-150811929
  files <- getSources

  -- Run haddock in the right sadbox.
  -- NOTE: you need this version of haddock:
  -- https://github.com/keera-studios/haddock/
  --
  -- TODO: adapt to use even the old version of haddock if the modified one is
  -- not present. We can test with haddock --csv-coverage and no input
  -- and check the exit code.
  let haddockArgs = [ "--csv-coverage", "--no-warnings" ] ++ files
  let cabalArgs   = [ "exec", "--", "haddock" ] ++ haddockArgs
  (code, out, _err) <- readProcessWithExitCode "cabal" cabalArgs ""

  -- Filter out coverage lines, and find those that denote undocumented
  -- modules.
  --
  -- TODO: is there a way to annotate a function as self-documenting,
  -- in the same way we do with ANN for hlint?
  let isCoverageLine     :: String -> Bool
      isCoverageLine     line = line =~ "^ *[0-9]+%"
  let isIncompleteModule :: String -> Bool
      isIncompleteModule line = not (line =~ "^ *100%")

  let coverageLines :: [String]
      coverageLines = filter isCoverageLine $ lines out

      incompleteModules :: [String]
      incompleteModules = filter isIncompleteModule coverageLines

  -- Based on the result of haddock, report errors and exit.
  -- Note that, unline haddock, this script does not
  -- output anything to stdout. It uses stderr instead
  -- (as it should).
  case (code, incompleteModules) of
    (ExitSuccess  , []) -> return ()
    (ExitFailure _, _)  -> exitFailure
    (_            , _)  -> do
      hPutStrLn stderr "The following modules are not fully documented:"
      mapM_ (hPutStrLn stderr) incompleteModules
      exitFailure

getSources :: IO [FilePath]
getSources = filter isHaskellFile <$> go "src"
  where
    go dir = do
      (dirs, files) <- getFilesAndDirectories dir
      (files ++) . concat <$> mapM go dirs

    isHaskellFile fp = isSuffixOf ".hs" fp || isSuffixOf ".lhs" fp

getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
  c <- map (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
  (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c

-- find-based implementation (not portable)
--
-- getSources :: IO [FilePath]
-- getSources = fmap lines $ readProcess "find" ["src/", "-iname", "*hs"] "" 
