{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Development.Shake.Install.CabalSimple
  ( CabalSimple(..)
  ) where

import Data.Monoid
import Development.Shake as Shake
import Development.Shake.Install.RequestResponse as Shake
import Development.Shake.Install.Cabal as Shake
import Development.Shake.Install.BuildDictionary as Shake
import Development.Shake.Install.PersistedEnvironment as Shake
import Development.Shake.Install.Utils (makePackageDescriptionPathsAbsolute)

import Distribution.PackageDescription (package, packageDescription, emptyBuildInfo, hsSourceDirs)
import Distribution.Simple (CompilerFlavor(GHC), PackageDB(SpecificPackageDB), pkgName)
import Distribution.Simple.Configure (configCompiler, configure, writePersistBuildConfig)
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Install (install)
import Distribution.Simple.Register (register)
import Distribution.Simple.Build (build)
import Distribution.Simple.Setup as Setup
import Distribution.Verbosity (normal)
import Distribution.Text (display)

import System.FilePath

data CabalSimple = CabalSimple

instance Cabal CabalSimple where
  -- | run the configuration action and write the resolved config out as 'setup-config'
  configAction _ filePath gdesc = do
    prefixTemplate <- fmap toPathTemplate $ requestOf penvPrefixDirectory
    pkgConfDir <- requestOf penvPkgConfDirectory

    let packageName = display . pkgName . package . packageDescription $ gdesc
    buildDirectory <- requestOf penvBuildDirectory
    sourceDir <- findSourceDirectory filePath

    traced "configure" $ do
      (_, config) <- configCompiler (Just GHC) Nothing Nothing defaultProgramConfiguration normal

      let config' =
            (defaultConfigFlags config)
               { configInstallDirs = mempty{prefix = Setup.Flag prefixTemplate, libsubdir = Setup.Flag (toPathTemplate "$pkgid")}
               , configPackageDBs = [Just buildDatabase]
               , configUserInstall = Setup.Flag True
               , configTests = Setup.Flag True}
--               , configProfLib = Setup.Flag True
--               , configProfExe = Setup.Flag True}

          commonBuildInfo = emptyBuildInfo{hsSourceDirs = [sourceDir] ++ [sourceDir </> "tests"]} --todo need to get this from gdesh
          hooked = (Just commonBuildInfo, []) -- TODO: array should have exe names and buildInfo
          buildDatabase = SpecificPackageDB pkgConfDir

      lbi <- configure (gdesc, hooked) config'

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
    pkgConfDir <- requestOf penvPkgConfDirectory

    traced "cabal register" $ do
      let flags = defaultRegisterFlags{regPackageDB = Setup.Flag (SpecificPackageDB pkgConfDir), regGenPkgConf = Setup.Flag (Just filePath)}
          desc = localPkgDescr lbi

      register desc lbi flags

