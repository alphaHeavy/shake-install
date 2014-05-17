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
      , desiredPackageDbs :: [String]
      }
  | ShakeBuild
      { desiredVerbosity :: Shake.Verbosity
      , desiredThreads   :: Maybe Int
      , desiredStaunch   :: Bool
      , desiredRecurse   :: Bool
      , desiredRoots     :: [String]
      , desiredPackages  :: [String]
      , desiredPackageDbs :: [String]
      }
  | ShakeInstall
      { desiredVerbosity :: Shake.Verbosity
      , desiredThreads   :: Maybe Int
      , desiredStaunch   :: Bool
      }

  | ShakeGhci
      { desiredVerbosity :: Shake.Verbosity
      , desiredThreads   :: Maybe Int
      , desiredArgs      :: [String]
      , desiredStaunch   :: Bool
      }
    deriving (Show, Data, Typeable)

shakeMode
  :: ShakeMode
shakeMode = modes
  [ ShakeClean
     { desiredVerbosity = Shake.Quiet &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     , desiredThreads   = Nothing &= name "j" &= name "jobs" &= explicit &= help "Number of parallel jobs"
     , desiredStaunch   = False &= name "k" &= name "keep-going" &= explicit &= help "Continue as much as possible after an error"
     } &= name "clean"
  , ShakeConfigure
     { desiredVerbosity = Shake.Quiet &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     , desiredPrefix    = "dist" </> "build" &= name "prefix" &= explicit &= help "Installation prefix"
     , desiredThreads   = Nothing &= name "j" &= name "jobs" &= explicit &= help "Number of parallel jobs"
     , desiredStaunch   = False &= name "k" &= name "keep-going" &= explicit &= help "Continue as much as possible after an error"
     , desiredPackageDbs= [] &= name "package-db" &= help "Additional Package DBs for finding dependencies."
     } &= name "configure"
  , ShakeBuild
     { desiredVerbosity = Shake.Quiet &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     , desiredThreads   = Nothing &= name "j" &= name "jobs" &= explicit &= help "Number of parallel jobs"
     , desiredStaunch   = False &= name "k" &= name "keep-going" &= explicit &= help "Continue as much as possible after an error"
     , desiredRecurse   = False &= name "r" &= name "recursive" &= explicit &= help "Recurse into directories looking for *.cabal files"
     , desiredRoots     = [] &= name "root" &= typDir &= explicit &= help "Additional dependency roots"
     , desiredPackages  = [] &= args &= typFile
     , desiredPackageDbs= [] &= name "package-db" &= help "Additional Package DBs for finding dependencies."
     } &= name "build" &= auto
  , ShakeInstall
     { desiredVerbosity = Shake.Quiet &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     , desiredThreads   = Nothing &= name "j" &= name "jobs" &= explicit &= help "Number of parallel jobs"
     , desiredStaunch   = False &= name "k" &= name "keep-going" &= explicit &= help "Continue as much as possible after an error"
     } &= name "install"
  , ShakeGhci
     { desiredVerbosity = Shake.Quiet &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     , desiredThreads   = Nothing &= name "j" &= name "jobs" &= explicit &= help "Number of parallel jobs"
     , desiredStaunch   = False &= name "k" &= name "keep-going" &= explicit &= help "Continue as much as possible after an error"
     , desiredArgs      = [] &= args
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

