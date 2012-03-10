{-# LANGUAGE DeriveDataTypeable #-}

module Development.Shake.Install.ShakeMode where

import Development.Shake as Shake
import Development.Shake.FilePath
import System.Console.CmdArgs

data ShakeMode
  = ShakeClean
      { desiredVerbosity :: Shake.Verbosity
      }
  | ShakeConfigure
      { desiredVerbosity :: Shake.Verbosity
      , desiredPrefix    :: FilePath
      }
  | ShakeBuild
      { desiredVerbosity :: Shake.Verbosity
      , desiredThreads   :: Maybe Int
      }
  | ShakeInstall
      { desiredVerbosity :: Shake.Verbosity
      }
    deriving (Show, Data, Typeable)

shakeMode :: ShakeMode
shakeMode = modes
  [ ShakeClean
     { desiredVerbosity = Shake.Normal &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     } &= name "clean"
  , ShakeConfigure
     { desiredVerbosity = Shake.Normal &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     , desiredPrefix    = "dist" </> "build" &= name "prefix" &= explicit &= help "Installation prefix"
     } &= name "configure"
  , ShakeBuild
     { desiredVerbosity = Shake.Normal &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     , desiredThreads   = Nothing &= name "j" &= name "jobs" &= explicit &= help "Number of parallel jobs"
     } &= name "build" &= auto
  , ShakeInstall
     { desiredVerbosity = Shake.Normal &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     } &= name "install"
  ] &= program "shake"

