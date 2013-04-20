{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Development.Shake.Install.Rules
  ( configureTheEnvironment
  , initializePackageConf
  , cabalConfigure
  , cabalBuild
  , cabalCopy
  , cabalRegister
  , ghcPkgRegister
  , buildTree
  , generatePackageMap
  ) where

import Control.Monad
import qualified Data.Map as Map
import Development.Shake as Shake
import Development.Shake.Install.RequestResponse as Shake
import Development.Shake.Install.BuildDictionary as Shake
import Development.Shake.Install.BuildTree as Shake
import Development.Shake.Install.Cabal as Shake
import Development.Shake.Install.CabalCustom as Shake
import Development.Shake.Install.CabalSimple as Shake
import Development.Shake.Install.GetBuildType as Shake
import Development.Shake.Install.PersistedEnvironment as Shake
import Development.Shake.Install.ShakeMode as Shake
import Development.Shake.Install.Utils as Shake
import Data.Generics.Uniplate.DataOnly
import qualified Language.Haskell.Interpreter as Hint

import Distribution.ModuleName (toFilePath)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils (getDirectoryContentsRecursive)
import Distribution.Verbosity
import Distribution.Text
import Language.Haskell.Extension

import System.Environment (getEnvironment)
import System.FilePath
import System.Directory as Dir
import System.Posix.Directory

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

-- | Create or reinitialize the packge.conf.d, should be performed as top level
-- step that no other rules depend on. Since the package.cache file is continually
-- updated, a dependency causes the whole tree to be rebuilt
initializePackageConf
  :: Rules ()
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

    when needsInit $
      system' "ghc-pkg" ["init", pkgConfDirectory]

getPackageDescription
  :: FilePath
  -> Action GenericPackageDescription
getPackageDescription filePath = do
  need [filePath]

  traced "readPackageDescription" $
    readPackageDescription normal filePath

noTemplateHaskell
  :: GetBuildType arg
  => arg
  -> Bool
noTemplateHaskell = not . any (== EnableExtension TemplateHaskell) . exts where
  exts = concat . fmap allExtensions . getBuildInfo

-- |
-- Dispatching function that selects between in-process
-- and cabal-install invocations to perform each build step
runCabalAction
  :: (Show arg, GetBuildType arg)
  => FilePath
  -> arg
  -> (forall a . Cabal a => a -> FilePath -> arg -> Action r)
  -> Action r
runCabalAction filePath lbi fun =
  case getBuildType lbi of
    -- template haskell that loads files from disk do not have the
    -- correct cwd set so relative loads fail
    Just Simple | noTemplateHaskell lbi ->
      fun CabalSimple filePath lbi

    _ ->
      fun CabalCustom filePath lbi

-- |
-- Parse a LocalBuildInfo for a configured package
getLocalBuildInfo
  :: FilePath
  -> Action LocalBuildInfo
getLocalBuildInfo filePath = do
  need [replaceFileName filePath "setup-config"]

  traced "getPersistBuildConfig" $
    getPersistBuildConfig (takeDirectory filePath)

-- |
-- Module paths don't come with an extension. This should
-- really be done as a batch operation rather than probing
-- each extension supported by cabal-install
tryNeedExtensions
  :: FilePath
  -> FilePath
  -> Action ()
tryNeedExtensions sourceDir modulePath = do
  let exts = [".hs", ".lhs", ".chs", ".hsc", ".x", ".y", ".ly", ".cpphs"]
  forM_ exts $ \ ext -> do
    let filePlusExt = sourceDir </> addExtension modulePath ext
    exists <- liftIO $ Dir.doesFileExist filePlusExt
    when exists $
      need [filePlusExt]

-- |
-- The main build rule, should be equivalent to "cabal configure"
cabalConfigure
  :: Rules ()
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
          -- are we referencing a dependency in this build tree?
          , Map.member pn packages
          -- but not a library in the same cabal file
          , pn /= (pkgName . package . packageDescription $ gdesc)
          ]
        testDeps =
          [ buildDir </> name </> "register"
          | (_, test) <- condTestSuites gdesc
          , Dependency pn@(PackageName name) _ <-  condTreeConstraints test
          -- are we referencing a dependency in this build tree?
          , Map.member pn packages
          -- but not a library in the same cabal file
          , pn /= (pkgName . package . packageDescription $ gdesc)
          ]

    need $ libDeps ++ exeDeps ++ testDeps

    fileExists <- case getBuildType gdesc of
      Just Custom -> liftIO $ Dir.doesFileExist filePath
      _           -> return False

    unless fileExists $
      runCabalAction filePath gdesc configAction

-- |
-- The main build rule, should be equivalent to "cabal build"
cabalBuild
  :: Rules ()
cabalBuild = "//package.conf.inplace" *> action where
  action filePath = do
    lbi <- getLocalBuildInfo filePath

    let desc = localPkgDescr lbi
        allModules
          | Just lib <- library desc = libModules lib : fmap exeModules (executables desc)
          | otherwise = fmap exeModules (executables desc)

    sourceDir <- findSourceDirectory filePath
    let attachSourceDir x = sourceDir </> x
        getAllSourceFiles = do
          contents <- getDirectoryContentsRecursive sourceDir
          return . fmap attachSourceDir . filter ("//*.hs" ?==) $ contents

    case library desc of
      Nothing -> need =<< liftIO getAllSourceFiles
      Just _  -> return ()

    let modulePaths = fmap toFilePath (concat allModules)

    mapM_ (tryNeedExtensions sourceDir) modulePaths

    runCabalAction filePath lbi buildAction

-- |
-- The main copy rule, should be equivalent to "cabal copy"
--
-- The target is a dummy file named 'copy' so we don't need to
-- calculate the actual files copied
cabalCopy
  :: Rules ()
cabalCopy = "//copy" *> action where
  action filePath = do
    need [replaceFileName filePath "package.conf.inplace"]

    lbi <- getLocalBuildInfo filePath

    runCabalAction filePath lbi copyAction

-- |
-- The main register script rule, should be equivalent to "cabal register"
--
-- This generates a registration script used by ghc-pkg register
cabalRegister
  :: Rules ()
cabalRegister = "//pkg.config" *> action where
  action filePath = do
    need [replaceFileName filePath "copy"]

    lbi <- getLocalBuildInfo filePath

    case library . localPkgDescr $ lbi of
      Just Library{libBuildInfo = BuildInfo{buildable = True}} ->
        runCabalAction filePath lbi registerAction

      _ -> do
        -- executables are not registered but it's convenient to pretend they are
        let name = display . pkgName . package . localPkgDescr $ lbi
        putLoud $ "Skipping registration script generation for executable package: " ++ name
        system' "touch" [filePath]

-- |
-- The main package register rule, should be equivalent to "ghc-pkg register"
--
-- "update" is used instead of "register" so incremental builds don't error out
-- when the package is added to the package.conf.d and cache
ghcPkgRegister
  :: Resource -- ^ singleton resource lock, "ghc-pkg register" must be serialized
  -> Rules ()
ghcPkgRegister res = "//register" *> action where
  action filePath = do
    let pkgConf = replaceFileName filePath "pkg.config"
    need [pkgConf]

    lbi <- getLocalBuildInfo filePath
    pkgConfDir <- requestOf penvPkgConfDirectory

    case library . localPkgDescr $ lbi of
      Just Library{libBuildInfo = BuildInfo{buildable = True}} ->
        withResource res 1 $
          system' "ghc-pkg" ["update", "-v0", "--global", "--user", "--package-db="++pkgConfDir, pkgConf]

      _ -> do
        -- executables are not registered but it's convenient to pretend they are
        let name = display . pkgName . package . localPkgDescr $ lbi
        putLoud $ "Skipping registration for executable package: " ++ name

    system' "touch" [filePath]

-- |
-- Generate a lookup dictionary used to map a PackageName to its location in the build tree
generatePackageMap
  :: Request BuildDictionary
  -> Maybe (Action (Response BuildDictionary))
generatePackageMap _ = Just action where
  action = do
    rootDir <- requestOf penvRootDirectory
    whatever <- apply1 (BuildChildren rootDir)
    let packages = [(source, buildFile) | BuildNode{buildFile, buildSources} <- universe whatever, source <- buildSources]
    pkgList <- forM packages $ \ (cabalFile, buildFile) -> do
      let cabalFile' = buildFile </> cabalFile
      gdesc <- getPackageDescription cabalFile'
      let packageName = pkgName . package . packageDescription $ gdesc
      return $! (packageName, cabalFile')

    return . Response . BuildDictionary $ Map.fromList pkgList

getPackageRegistrationFiles
  :: FilePath
  -> FilePath
  -> [FilePath]
  -> Action [FilePath]
getPackageRegistrationFiles buildDir dir =
  mapM $ \ pkg -> do
    gdesc <- getPackageDescription $ dir </> pkg
    let packageName = display . pkgName . package . packageDescription $ gdesc
    return $! buildDir </> packageName </> "register"

-- | Recurse the build tree looking for cabal packages to build
buildTree
  :: Resource
  -> BuildStyle
  -> BuildTree
  -> Maybe (Action BuildNode)
buildTree _ BuildRecursiveWildcard{} (BuildChildren dir) = Just action where
  action = do
    rootDir <- requestOf penvRootDirectory
    buildDir <- requestOf penvBuildDirectory

    let emptyNode = BuildNode
          { buildFile     = rootDir </> dir
          , buildChildren = []
          , buildSources  = []
          , buildRegister = []
          }

        processStream :: BuildNode -> DirStream -> Action BuildNode
        processStream buildNode dirStream = do
          entry <- liftIO $ readDirStream dirStream
          if null entry
            then return buildNode
            else do
              isDir <- liftIO $ doesDirectoryExist entry
              buildNode' <- if isDir && entry /= "." && entry /= ".."
                then do
                  child <- apply1 $ BuildChildren (dir </> entry)
                  return buildNode{buildChildren = child:buildChildren buildNode}
                else if takeExtension entry == ".cabal"
                  then do
                    registrationFiles <- getPackageRegistrationFiles buildDir dir [entry]
                    return buildNode
                      { buildSources  = entry:buildSources buildNode
                      , buildRegister = buildRegister buildNode ++ registrationFiles
                      }
                  else return buildNode

              liftIO $ print buildNode'

              processStream buildNode' dirStream

    -- no lifted bracket for Action, hope for the best
    dirStream <- liftIO $ openDirStream (rootDir </> dir)
    buildNode <- processStream emptyNode dirStream
    liftIO $ closeDirStream dirStream

    return buildNode

buildTree _ BuildWithExplicitPaths{..} bc@(BuildChildren dir) = Just action where
  action = do
    rootDir <- requestOf penvRootDirectory
    buildDir <- requestOf penvBuildDirectory
    currentDir <- liftIO getCurrentDirectory

    -- only depend on other root directories once
    children' <- if rootDir /= rootDir </> dir
      then apply $ filter (/= bc) $ fmap (\ x -> BuildChildren (currentDir </> x)) buildDepends
      else return []

    registrationFiles <- getPackageRegistrationFiles buildDir currentDir buildCabalFiles

    return $! BuildNode
      { buildFile     = rootDir </> dir
      , buildChildren = children'
      , buildSources  = buildCabalFiles
      , buildRegister = registrationFiles
      }


-- | Walk the tree evaluating Shakefile.hs to discover .cabal files to build
buildTree hintResource BuildViaShakefile{} (BuildChildren dir) = Just action where
  action = do
    rootDir <- requestOf penvRootDirectory
    buildDir <- requestOf penvBuildDirectory

    let shakefile = rootDir </> dir </> "Shakefile.hs"

    need [shakefile]

    res <- withResource hintResource 1 $ liftIO . Hint.runInterpreter $ do
      Hint.reset
      Hint.loadModules [shakefile]
      Hint.setImports ["Prelude", "Main"]
      Hint.interpret "(Main.children, Main.sources)" (undefined :: ([String], [String]))

    case res of
      Left err -> fail $ "Error while interpreting " ++ shakefile ++ ": " ++ show err
      Right (children, sources) -> do
        children' <- apply $ fmap (\ x -> BuildChildren $ dir </> x) children
        registrationFiles <- getPackageRegistrationFiles buildDir dir sources

        return $! BuildNode
          { buildFile     = rootDir </> dir
          , buildChildren = children'
          , buildSources  = sources
          , buildRegister = registrationFiles
          }
