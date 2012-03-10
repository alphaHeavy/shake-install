module Development.Shake.Install.GetBuildType where

import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

class GetBuildType a where
  getBuildType :: a -> Maybe BuildType

instance GetBuildType LocalBuildInfo where
  getBuildType = buildType . localPkgDescr

instance GetBuildType GenericPackageDescription where
  getBuildType = buildType . packageDescription

