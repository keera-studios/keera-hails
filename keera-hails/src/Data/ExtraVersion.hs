module Data.ExtraVersion where

data Version = Version
 { vMajor  :: Int
 , vMinor  :: Int
 , vStatus :: VersionStatus
 , vIter   :: Int
 }
 deriving (Eq, Ord, Show, Read)

data VersionStatus = None
                   | Alpha
                   | Beta
                   | ReleaseCandidate
                   | Final
 deriving (Eq, Ord, Show, Enum, Read)

versionToString :: Version -> String
versionToString v = mj ++ "." ++ mn ++ "-" ++ st ++ it
  where mj = show $ vMajor v
        mn = show $ vMinor v
        st = case vStatus v of
              Alpha            -> "alpha"
              Beta             -> "beta"
              ReleaseCandidate -> "rc"
              Final            -> "r"
              None             -> ""
        it = show $ vIter v
