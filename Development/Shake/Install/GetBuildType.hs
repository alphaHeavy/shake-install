module Development.Shake.Install.GetBuildType
  ( GetBuildType(..)
  ) where

import Data.List (nub)
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

class GetBuildType a where
  getBuildType :: a -> Maybe BuildType
  getBuildInfo :: a -> [BuildInfo]

instance GetBuildType LocalBuildInfo where
  getBuildType = buildType . localPkgDescr
  getBuildInfo = allBuildInfo . localPkgDescr

instance GetBuildType GenericPackageDescription where
  getBuildType = buildType . packageDescription
  getBuildInfo = nub . getGenericBuildInfo

getGenericBuildInfo :: GenericPackageDescription -> [BuildInfo]
getGenericBuildInfo x = allInfo x ++ regularInfo x where
  allInfo x = case condLibrary x of
    Just node -> (libBuildInfo . condTreeData $ node) : exeInfo x
    Nothing   -> exeInfo x

  exeInfo = fmap exeBuildInfo . condExecutables

  exeBuildInfo (_, node) =  buildInfo . condTreeData  $ node

  regularInfo = allBuildInfo . packageDescription

