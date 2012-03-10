{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Development.Shake.Install.Rules where

import Control.Monad
import Control.Monad.Reader
import Data.Map as Map
import Development.Shake as Shake
import Development.Shake.Install.RequestResponse as Shake
import Development.Shake.Install.BuildDictionary as Shake
import Development.Shake.Install.BuildTree as Shake
import Development.Shake.Install.Cabal as Shake
import Development.Shake.Install.CabalCustom as Shake
import Development.Shake.Install.CabalSimple as Shake
import Development.Shake.Install.Exceptions as Shake
import Development.Shake.Install.GetBuildType as Shake
import Development.Shake.Install.PersistedEnvironment as Shake
import Development.Shake.Install.ShakeMode as Shake
import Development.Shake.Install.Utils as Shake
import Data.Generics.Uniplate.DataOnly

import Distribution.ModuleName (toFilePath)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Verbosity
import Distribution.Text

import System.Environment (getEnvironment)
import System.FilePath

import Data.Data

import System.Console.CmdArgs
import System.Directory as Dir

import GHC.Conc (getNumProcessors)

configureTheEnvironment
  :: (FilePath, FilePath)
  -> ShakeMode
  -> Request PersistedEnvironment
  -> Maybe (Action (Response PersistedEnvironment))
configureTheEnvironment (rootDir, _) sm _ = Just action where
  action = do
    env <- liftIO getEnvironment
    let penv = PersistedEnvironment
          { penvEnvironment      = env
          , penvRootDirectory    = rootDir
          , penvBuildDirectory   = rootDir </> "build"
          , penvPrefixDirectory  = pfx
          , penvPkgConfDirectory = rootDir </> "build" </> "package.conf.d"
          }

        pfx | ShakeConfigure{desiredPrefix = prefix} <- sm = rootDir </> prefix
            | otherwise = rootDir </> "build" </> "dist"

    return $! Response penv

initializePackageConf :: Rules ()
initializePackageConf = "//package.conf.d/package.cache" *> action where
  action filePath = do
    let pkgConfDirectory = takeDirectory filePath

    needsInit <- liftIO $ do
      hasPkgCache <- Dir.doesFileExist filePath
      if hasPkgCache
        then return False
        else do
          corruptPackageConf <- doesDirectoryExist pkgConfDirectory
          when corruptPackageConf $
            removeDirectoryRecursive pkgConfDirectory

          return True

    when needsInit $ do
      system' "ghc-pkg" ["init", pkgConfDirectory]

getPackageDescription
  :: FilePath
  -> Action GenericPackageDescription
getPackageDescription filePath = do
  need [filePath]

  traced "readPackageDescription" $
    readPackageDescription normal filePath

cabalConfigure :: Rules ()
cabalConfigure = "//setup-config" *> action where
  action filePath = do
    sourceDir <- findSourceDirectory filePath
    buildDir <- requestOf penvBuildDirectory

    let fixupPackageDesc = fixupGenericPaths sourceDir

    packages <- requestOf unBuildDict -- (Request :: Request BuildDictionary)
    cabalPath <- findCabalFile filePath
    gdesc <- fmap fixupPackageDesc $ getPackageDescription cabalPath

    let libDeps = case condLibrary gdesc of
          Just lib ->
            [ buildDir </> name </> "register"
            | Dependency pn@(PackageName name) _ <- condTreeConstraints lib
            , Map.member pn packages ]
          Nothing ->
            []

        exeDeps =
          [ buildDir </> name </> "register"
          | (_, exe) <- condExecutables gdesc
          , Dependency pn@(PackageName name) _ <-  condTreeConstraints exe
          , Map.member pn packages ]

    need $ libDeps ++ exeDeps

    runCabalAction filePath gdesc configAction

runCabalAction
  :: GetBuildType arg
  => FilePath
  -> arg
  -> (forall a . Cabal a => a -> FilePath -> arg -> Action r)
  -> Action r
runCabalAction filePath lbi fun =
  case getBuildType lbi of
    Just Simple -> fun CabalSimple filePath lbi
    _           -> fun CabalCustom filePath lbi

getLocalBuildInfo :: FilePath -> Action LocalBuildInfo
getLocalBuildInfo filePath = do
    need [replaceFileName filePath "setup-config"]

    traced "getPersistBuildConfig" $
      getPersistBuildConfig (takeDirectory filePath)

tryNeedExtensions :: FilePath -> FilePath -> Action ()
tryNeedExtensions filePath modulePath = do
  sourceDir <- findSourceDirectory filePath

  let exts = [".hs", ".lhs", ".chs", ".hsc", ".x", ".y", ".ly", ".cpphs"]
  forM_ exts $ \ ext -> do
    let filePlusExt = sourceDir </> addExtension modulePath ext
    exists <- liftIO $ Dir.doesFileExist filePlusExt
    when exists $
      need [filePlusExt] 

cabalBuild :: Rules ()
cabalBuild = "//package.conf.inplace" *> action where
  action filePath = do
    lbi <- getLocalBuildInfo filePath

    let desc = localPkgDescr lbi
        allModules
          | Just lib <- library desc = libModules lib : fmap exeModules (executables desc)
          | otherwise = fmap exeModules (executables desc)
        modulePaths = fmap toFilePath (concat allModules)

    mapM_ (tryNeedExtensions filePath) modulePaths

    runCabalAction filePath lbi buildAction

cabalCopy :: Rules ()
cabalCopy = "//copy" *> action where
  action filePath = do
    need [replaceFileName filePath "package.conf.inplace"]

    lbi <- getLocalBuildInfo filePath

    runCabalAction filePath lbi copyAction

cabalRegister :: Rules ()
cabalRegister = "//pkg.config" *> action where
  action filePath = do
    need [replaceFileName filePath "copy"]

    lbi <- getLocalBuildInfo filePath

    case library . localPkgDescr $ lbi of
      Just _ ->
        runCabalAction filePath lbi registerAction

      Nothing ->
        system' "touch" [filePath]

ghcPkgRegister :: Resource -> Rules ()
ghcPkgRegister res = "//register" *> action where
  action filePath = do
    let pkgConf = replaceFileName filePath "pkg.config"
    need [pkgConf]

    lbi <- getLocalBuildInfo filePath
    pkgConfDir <- requestOf penvPkgConfDirectory

    case library . localPkgDescr $ lbi of
      Just _ ->
        withResource res 1 $
          system' "ghc-pkg" ["update", "-v0", "--global", "--user", "--package-conf="++pkgConfDir, pkgConf]

      Nothing ->
        return ()

    system' "touch" [filePath]

