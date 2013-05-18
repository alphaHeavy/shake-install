{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Shake.Install.Utils where

import Control.Lens
import Control.Monad (when)
import Development.Shake as Shake (Action, traced)
import Development.Shake.Install.PersistedEnvironment as Shake
import Development.Shake.Install.RequestResponse (requestOf)
import qualified Development.Shake.Install.PackageDescription as Shake
import Distribution.PackageDescription
import System.Directory
import System.Exit
import System.FilePath
import System.Process

systemWithDirectory
  :: String
  -> String
  -> [String]
  -> Shake.Action ()
systemWithDirectory command cwd args = do
  env <- requestOf penvEnvironment

  traced command $ do
    let cp = (proc command args){cwd = Just cwd, env = Just env}

    putStrLn $ unwords (cwd:command:args)
    (_, _, _, h) <- createProcess cp

    -- putLoud command
    -- res <- traced ("system " ++ cmd) $ rawSystem path2 args
    res <- waitForProcess h
    when (res /= ExitSuccess) $
      error $ "System command failed:\n" ++ "foo"

fixupGenericPaths
  :: FilePath
  -> GenericPackageDescription
  -> GenericPackageDescription
fixupGenericPaths filePath gdesc@GenericPackageDescription{packageDescription} =
  gdesc{packageDescription = makePackageDescriptionPathsAbsolute filePath packageDescription}

fixupHsSourceDirs :: FilePath -> GenericPackageDescription -> GenericPackageDescription
fixupHsSourceDirs sourceDirectory = lbi . ebi . tbi . bmi where
  lbi = Shake.libraryBuildInfo     . Shake._hsSourceDirs %~ makeAbsolute
  ebi = Shake.executableBuildInfos . Shake._hsSourceDirs %~ makeAbsolute
  tbi = Shake.testSuiteBuildInfos  . Shake._hsSourceDirs %~ makeAbsolute
  bmi = Shake.benchmarkBuildInfos  . Shake._hsSourceDirs %~ makeAbsolute
  makeAbsolute fileName
    | Prelude.null fileName = fileName
    | otherwise             = combine sourceDirectory fileName

makePackageDescriptionPathsAbsolute
  :: FilePath
  -> PackageDescription
  -> PackageDescription
makePackageDescriptionPathsAbsolute sourceDirectory desc@PackageDescription{..} = hookedDescription where
  hookedDescription = updatePackageDescription hooked updatedDescription
  commonBuildInfo = emptyBuildInfo{hsSourceDirs = [sourceDirectory]}
  hooked = (Just commonBuildInfo, []) -- TODO: array should have exe names and buildInfo
  updatedDescription = desc
    { library = fmap updLib library
    , executables = fmap updExe executables
    , licenseFile = makeAbsolute licenseFile
    , dataDir = makeAbsolute dataDir
    }
  updLib lib@Library{libBuildInfo} = lib{libBuildInfo = fixupDirectoryPaths libBuildInfo}
  updExe exe@Executable{buildInfo} = exe{buildInfo = fixupDirectoryPaths buildInfo}
  fixupDirectoryPaths bi@BuildInfo{cSources, extraLibDirs, includeDirs, hsSourceDirs} = bi
    { cSources = fmap makeAbsolute cSources
    , extraLibDirs = fmap makeAbsolute extraLibDirs
    , Distribution.PackageDescription.includeDirs = fmap makeAbsolute includeDirs
    , hsSourceDirs = fmap makeAbsolute hsSourceDirs ++ [sourceDirectory]
    }
  makeAbsolute fileName
    | Prelude.null fileName = fileName
    | otherwise             = combine sourceDirectory fileName

findDirectoryBounds
  :: IO (FilePath, FilePath)
findDirectoryBounds = step1 =<< getCurrentDirectory where
  step1 dir = do
    fileFound <- doesFileExist (dir </> "Shakefile.hs")
    dbFound <- doesFileExist (dir </> ".shake.database")

    let parentDir = takeDirectory dir
    case (fileFound, dbFound) of
      (_, True)                     -> return (dir, dir)
      (True, _)                     -> step2 (dir, dir) parentDir 
      (False, _) | dir /= parentDir -> step1 parentDir

      -- No Shakefile.hs found in any parent directories, assume
      -- the .cabal files are specified on the command line
      -- or that we're going to recurse the source tree
      -- searching for them
      _ -> do
        cwd <- getCurrentDirectory
        return (cwd, cwd)

  step2 best@(_, top) dir = do
    fileFound <- doesFileExist (dir </> "Shakefile.hs")
    dbFound <- doesFileExist (dir </> ".shake.database")

    let parentDir = takeDirectory dir
    case (fileFound, dbFound) of
      (_, True)                    -> return (dir, top)
      (True, _) | dir /= parentDir -> step2 (dir, top) parentDir
                | otherwise        -> return (dir, top)
      _                            -> return best

