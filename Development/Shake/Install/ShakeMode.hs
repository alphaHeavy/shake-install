{-# LANGUAGE DeriveDataTypeable #-}

module Development.Shake.Install.ShakeMode where

import Development.Shake as Shake
import Development.Shake.FilePath
import System.Console.CmdArgs

data ShakeMode
  = ShakeClean
      { desiredVerbosity :: Shake.Verbosity
      , desiredThreads   :: Maybe Int
      , desiredStaunch   :: Bool
      }
  | ShakeConfigure
      { desiredVerbosity :: Shake.Verbosity
      , desiredPrefix    :: FilePath
      , desiredThreads   :: Maybe Int
      , desiredStaunch   :: Bool
      }
  | ShakeBuild
      { desiredVerbosity :: Shake.Verbosity
      , desiredThreads   :: Maybe Int
      , desiredStaunch   :: Bool
      , desiredRecurse   :: Bool
      , desiredRoots     :: [String]
      , desiredPackages  :: [String]
      }
  | ShakeInstall
      { desiredVerbosity :: Shake.Verbosity
      , desiredThreads   :: Maybe Int
      , desiredStaunch   :: Bool
      }

  | ShakeGhci
      { desiredArgs      :: [String]
      }
    deriving (Show, Data, Typeable)

shakeMode
  :: ShakeMode
shakeMode = modes
  [ ShakeClean
     { desiredVerbosity = Shake.Normal &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     , desiredThreads   = Nothing &= name "j" &= name "jobs" &= explicit &= help "Number of parallel jobs"
     , desiredStaunch   = False &= name "k" &= name "keep-going" &= explicit &= help "Continue as much as possible after an error"
     } &= name "clean"
  , ShakeConfigure
     { desiredVerbosity = Shake.Normal &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     , desiredPrefix    = "dist" </> "build" &= name "prefix" &= explicit &= help "Installation prefix"
     , desiredThreads   = Nothing &= name "j" &= name "jobs" &= explicit &= help "Number of parallel jobs"
     , desiredStaunch   = False &= name "k" &= name "keep-going" &= explicit &= help "Continue as much as possible after an error"
     } &= name "configure"
  , ShakeBuild
     { desiredVerbosity = Shake.Normal &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     , desiredThreads   = Nothing &= name "j" &= name "jobs" &= explicit &= help "Number of parallel jobs"
     , desiredStaunch   = False &= name "k" &= name "keep-going" &= explicit &= help "Continue as much as possible after an error"
     , desiredRecurse   = False &= name "r" &= name "recursive" &= explicit &= help "Recurse into directories looking for *.cabal files"
     , desiredRoots     = [] &= name "root" &= typDir &= explicit &= help "Additional dependency roots"
     , desiredPackages  = [] &= args &= typFile
     } &= name "build" &= auto
  , ShakeInstall
     { desiredVerbosity = Shake.Normal &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     , desiredThreads   = Nothing &= name "j" &= name "jobs" &= explicit &= help "Number of parallel jobs"
     , desiredStaunch   = False &= name "k" &= name "keep-going" &= explicit &= help "Continue as much as possible after an error"
     } &= name "install"
  , ShakeGhci
     { desiredArgs      = [] &= args
     } &= name "ghci"
  ] &= program "shake"


data BuildStyle
  = BuildViaShakefile
      {
      }
  | BuildRecursiveWildcard
      {
      }
  | BuildWithExplicitPaths
      { buildCabalFiles :: [FilePath]
      , buildDepends    :: [FilePath]
      }

