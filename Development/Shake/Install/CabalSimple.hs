{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Development.Shake.Install.CabalSimple
  ( CabalSimple(..)
  ) where

import Control.Applicative
import Data.Monoid
import Development.Shake as Shake
import Development.Shake.Install.Cabal as Shake
import Development.Shake.Install.BuildDictionary as Shake
import Development.Shake.Install.PersistedEnvironment as Shake
import Development.Shake.Install.Utils (fixupHsSourceDirs, makePackageDescriptionPathsAbsolute)

import Distribution.PackageDescription (package, packageDescription)
import Distribution.Simple (PackageDB(SpecificPackageDB), pkgName)
import Distribution.Simple.Configure (configure, writePersistBuildConfig)
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Install (install)
import Distribution.Simple.Register (register)
import Distribution.Simple.Build (build)
import Distribution.Simple.Program.Builtin (builtinPrograms)
import Distribution.Simple.Program.Db (restoreProgramDb)
import Distribution.Simple.Setup as Setup
import Distribution.Text (display)

import System.FilePath

data CabalSimple = CabalSimple

instance Cabal CabalSimple where
  -- | run the configuration action and write the resolved config out as 'setup-config'
  configAction _ filePath gdesc = do
    ourEnv <- apply1 (PersistedEnvironmentRequest ())
    let prefixTemplate = toPathTemplate $ penvPrefixDirectory ourEnv
        pkgConfDir     = penvPkgConfDirectory ourEnv
        packageName = display . pkgName . package . packageDescription $ gdesc
        buildDirectory = penvBuildDirectory ourEnv

    sourceDir <- findSourceDirectory filePath
    let programDbPath = pkgConfDir </> "program.db"
    need [programDbPath]
    config <- restoreProgramDb builtinPrograms . read <$> readFile' programDbPath

    traced "configure" $ do
      let config' =
            (defaultConfigFlags config)
               { configInstallDirs = mempty{prefix = Setup.Flag prefixTemplate, libsubdir = Setup.Flag (toPathTemplate "$pkgid")}
               , configPackageDBs = [Just buildDatabase]
               , configUserInstall = Setup.Flag True
               , configTests = Setup.Flag True}
--               , configProfLib = Setup.Flag True
--               , configProfExe = Setup.Flag True}

          buildDatabase = SpecificPackageDB pkgConfDir

      let gdesc' = fixupHsSourceDirs sourceDir gdesc
      lbi <- configure (gdesc', (Nothing, [])) config'

      let lbi' = lbi{localPkgDescr = updatedDesc, buildDir = buildDirectory </> packageName}
          updatedDesc = makePackageDescriptionPathsAbsolute sourceDir (localPkgDescr lbi)

      writePersistBuildConfig (takeDirectory filePath) lbi'

  buildAction _ filePath lbi = do
    let desc = localPkgDescr lbi
        buildFlags = defaultBuildFlags{buildDistPref = Setup.Flag (takeDirectory filePath)}

    traced "build" $
      build desc lbi buildFlags knownSuffixHandlers

  copyAction _ filePath lbi = do
    traced "cabal install" $ do
      let flags = defaultCopyFlags -- {copyDest = Setup.Flag (CopyTo (takeDirectory filePath)), copyDistPref = Setup.Flag "xxxx"}
          desc = localPkgDescr lbi

      install desc lbi flags

    system' "touch" [filePath]

  registerAction _ filePath lbi = do
    pkgConfDir <- penvPkgConfDirectory <$> apply1 (PersistedEnvironmentRequest ())

    traced "cabal register" $ do
      let flags = defaultRegisterFlags{regPackageDB = Setup.Flag (SpecificPackageDB pkgConfDir), regGenPkgConf = Setup.Flag (Just filePath)}
          desc = localPkgDescr lbi

      register desc lbi flags

