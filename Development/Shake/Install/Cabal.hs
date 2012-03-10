module Development.Shake.Install.Cabal where

import Development.Shake (Action)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)

class Cabal a where
  configAction   :: a -> FilePath -> GenericPackageDescription -> Action ()
  buildAction    :: a -> FilePath -> LocalBuildInfo -> Action ()
  copyAction     :: a -> FilePath -> LocalBuildInfo -> Action ()
  registerAction :: a -> FilePath -> LocalBuildInfo -> Action ()

