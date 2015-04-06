{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

module Development.Shake.Install.ShakeMode where

import qualified Development.Shake as Shake
import Development.Shake.FilePath
import Options.Applicative
import Data.Data (Data)
import Data.Typeable (Typeable)

desiredStaunch :: ShakeMode -> Bool
desiredStaunch (ShakeClean CleanOpts{cleanStaunch}) = cleanStaunch
desiredStaunch (ShakeConfigure ConfigureOpts{configureStaunch}) = configureStaunch
desiredStaunch (ShakeBuild BuildOpts{buildStaunch}) = buildStaunch
desiredStaunch (ShakeInstall InstallOpts{installStaunch}) = installStaunch
desiredStaunch (ShakeGhci GhciOpts{ghciStaunch}) = ghciStaunch

desiredPackageDbs :: ShakeMode -> [String]
desiredPackageDbs (ShakeConfigure ConfigureOpts{configurePackageDbs}) = configurePackageDbs
desiredPackageDbs (ShakeBuild BuildOpts{buildPackageDbs}) = buildPackageDbs
desiredPackageDbs _ = []

data CleanOpts = CleanOpts
  { cleanStaunch   :: Bool
  } deriving (Show, Data, Typeable)
data ConfigureOpts = ConfigureOpts
  { desiredPrefix    :: FilePath
  , configureStaunch :: Bool
  , configurePackageDbs :: [String]
  } deriving (Show, Data, Typeable)
data BuildOpts = BuildOpts
  { buildThreads    :: Maybe Int
  , buildStaunch    :: Bool
  , desiredRecurse  :: Bool
  , desiredRoots    :: [String]
  , desiredPackages :: [String]
  , buildPackageDbs :: [String]
  } deriving (Show, Data, Typeable)
data InstallOpts = InstallOpts
  { installStaunch :: Bool
  } deriving (Show, Data, Typeable)
data GhciOpts = GhciOpts
  { ghciStaunch :: Bool
  , desiredArgs    :: [String]
  } deriving (Show, Data, Typeable)

data ShakeMode
  = ShakeClean CleanOpts
  | ShakeConfigure ConfigureOpts
  | ShakeBuild BuildOpts
  | ShakeInstall InstallOpts
  | ShakeGhci GhciOpts
  deriving (Show, Data, Typeable)

data ShakeInstallOpts = ShakeInstallOpts
  { desiredVerbosity :: Shake.Verbosity
  , shakeMode :: ShakeMode
  }

verbosityParser :: Parser Shake.Verbosity
verbosityParser = option auto ( short 'v' <> long "verbose" <> value Shake.Quiet <> help "Desired verbosity level")
jobsParser :: Parser (Maybe Int)
jobsParser = optional $ option auto (short 'j' <> long "jobs" <> help "Number of parallel jobs")
staunchParser :: Parser Bool
staunchParser = switch (short 'k' <> long "keep-going" <> help "Continue as much as possible after an error")

programParser :: ParserInfo ShakeInstallOpts
programParser = info ((ShakeInstallOpts <$> verbosityParser <*> shakeModeParser) <**> helper)
                     (progDesc "The shake program" <> fullDesc)

shakeModeParser :: Parser ShakeMode
shakeModeParser = hsubparser $
    command "clean" (info (ShakeClean <$> (CleanOpts <$> staunchParser)) $ progDesc "clean")
 <> command "configure" (info (ShakeConfigure <$> (ConfigureOpts
   <$> option str (value ("dist" </> "build") <> long "prefix" <> help "Installation prefix")
   <*> staunchParser
   <*> many (option str (long "package-db" <> help "Additional Package DBs for finding dependencies."))
     )) $ progDesc "configure")
 <> command "build" (info (ShakeBuild <$> (BuildOpts
   <$> jobsParser
   <*> staunchParser
   <*> switch (short 'r' <> long "recursive" <> help "Recurse into directories looking for *.cabal files")
   <*> many (option str (long "root" <> help "Additional dependency roots"))
   <*> many (option str (long "packages"))
   <*> many (option str (long "package-db" <> help "Additional Package DBs for finding dependencies."))
     )) $ progDesc "build")
 <> command "install" (info (ShakeInstall <$> (InstallOpts
   <$> staunchParser
     )) $ progDesc "install")
 <> command "ghci" (info (ShakeGhci <$> (GhciOpts
   <$> staunchParser
   <*> pure []
     )) $ progDesc "ghci")


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

