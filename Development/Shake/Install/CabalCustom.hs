module Development.Shake.Install.CabalCustom
  ( CabalCustom(..)
  ) where

import Control.Applicative
import Development.Shake
import Development.Shake.Install.BuildDictionary as Shake
import Development.Shake.Install.Cabal
import Development.Shake.Install.PersistedEnvironment as Shake
import Development.Shake.Install.Utils as Shake
import System.FilePath

-- |
-- For all other build-types just call out to cabal-install for the time being. When the dependency
-- on cabal-install is dropped these will need to be reimplemented via the package Setup.hs/lhs file
data CabalCustom = CabalCustom

instance Cabal CabalCustom where
  configAction _ filePath _ = do
    prefixDir  <- penvPrefixDirectory <$> askOracle (PersistedEnvironmentRequest ())
    pkgConfDir <- penvPkgConfDirectory <$> askOracle (PersistedEnvironmentRequest ())

    sourceDir <- findSourceDirectory filePath
    let args = ["configure", "-v0", "--prefix="++prefixDir, "--global", "--disable-shared", "--disable-library-profiling", "--disable-executable-profiling", "--user", "--package-db="++pkgConfDir, "--builddir="++takeDirectory filePath]
    systemWithDirectory "cabal" sourceDir args

  buildAction _ filePath _ = do
    sourceDir <- findSourceDirectory filePath
    systemWithDirectory "cabal" sourceDir ["build", "-v1", "--builddir="++takeDirectory filePath]

  copyAction _ filePath _ = do
    sourceDir <- findSourceDirectory filePath
    systemWithDirectory "cabal" sourceDir ["copy", "-v0", "--builddir="++takeDirectory filePath]
    system' "touch" [filePath]

  registerAction _ filePath _ = do
    sourceDir <- findSourceDirectory filePath
    systemWithDirectory "cabal" sourceDir ["register", "-v0", "--gen-pkg-config="++filePath, "--builddir="++takeDirectory filePath]

