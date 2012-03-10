{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Monad
import Control.Monad.Reader
import Data.Monoid
import Data.Map as Map
import Development.Shake as Shake
import Development.Shake.Install.RequestResponse as Shake
import Data.Generics.Uniplate.DataOnly
-- import Development.Shake.FilePath

import Distribution.ModuleName (toFilePath)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple
import Distribution.Simple.Configure
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Install
import Distribution.Simple.Register
import Distribution.Simple.Build as Build
import Distribution.Simple.Setup as Setup
import Distribution.Verbosity
import Distribution.Text

import System.Environment (getEnvironment)
import System.FilePath

import Data.Data
import Data.Binary
import Control.DeepSeq
import Data.Hashable

import System.Console.CmdArgs
import System.Directory as Dir
import System.Exit
import System.Process


import Control.Exception (Exception(..), SomeException(..))
import Data.IORef (IORef, newIORef, writeIORef)
import GHC.Conc (getNumProcessors, setUncaughtExceptionHandler)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Signals (raiseSignal, sigABRT)

-- | a dummy exception to initialize the global IORef with
data NoException = NoException deriving (Typeable)

instance Show NoException where
  show _ = "<<no exception>>"

instance Exception NoException

-- | storage for the last exception
lastException :: IORef SomeException
{-# NOINLINE lastException #-}
lastException = unsafePerformIO . newIORef $ SomeException NoException

-- | when no catch frame handles an exception dump core and terminate the process
uncaughtExceptionHandler :: SomeException -> IO ()
{-# NOINLINE uncaughtExceptionHandler #-}
uncaughtExceptionHandler !e = do
  writeIORef lastException e
  hPutStrLn stderr $ "Unhandled exception: " ++ show e
  raiseSignal sigABRT

setDefaultUncaughtExceptionHandler :: IO ()
setDefaultUncaughtExceptionHandler =
  setUncaughtExceptionHandler uncaughtExceptionHandler

data PersistedEnvironment = PersistedEnvironment
  { penvEnvironment      :: [(String, String)]
  , penvRootDirectory    :: FilePath
  , penvBuildDirectory   :: FilePath
  , penvPrefixDirectory  :: FilePath
  , penvPkgConfDirectory :: FilePath
  } deriving (Show, Eq, Ord, Typeable)

instance Binary PersistedEnvironment where
  put PersistedEnvironment{..} = do
    put penvEnvironment
    put penvRootDirectory
    put penvBuildDirectory
    put penvPrefixDirectory
    put penvPkgConfDirectory

  get =
    liftM5 PersistedEnvironment get get get get get

instance Hashable PersistedEnvironment where
  hash PersistedEnvironment{..} = hash
    ( penvEnvironment
    , penvRootDirectory
    , penvBuildDirectory
    , penvPrefixDirectory
    , penvPkgConfDirectory)

instance NFData PersistedEnvironment where
  rnf PersistedEnvironment{..} =
    rnf penvEnvironment `seq`
    rnf penvRootDirectory `seq`
    rnf penvBuildDirectory `seq`
    rnf penvPrefixDirectory `seq`
    rnf penvPkgConfDirectory `seq`
    ()

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


requestOf
  :: forall a b . Rule (Request a) (Response a)
  => (a -> b) -- ^ record field selector
  -> Action b
requestOf fun = do
  fmap (fun . unResponse) $ apply1 (Request :: Request a)

systemWithDirectory :: String -> String -> [String] -> Action ()
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

instance NFData PackageIdentifier where
  rnf (PackageIdentifier x1 x2)
    = ((rnf x1) `seq` ((rnf x2) `seq` ()))

instance NFData PackageName where
  rnf (PackageName x1) = ((rnf x1) `seq` ())

instance Hashable (Map PackageName FilePath) where
  hash = hash . toList

instance Hashable PackageName where
  hash = hash . display

instance Binary PackageName where
  get = fmap PackageName get
  put = put . display

newtype BuildDictionary = BuildDictionary{unBuildDict :: Map PackageName FilePath}
  deriving (Read, Show, Eq, Typeable, NFData, Binary, Hashable)

deriving instance Typeable PackageName

fixupGenericPaths
  :: FilePath
  -> GenericPackageDescription
  -> GenericPackageDescription
fixupGenericPaths filePath gdesc@GenericPackageDescription{packageDescription} =
  gdesc{packageDescription = makePackageDescriptionPathsAbsolute filePath packageDescription}

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
    | otherwise             = System.FilePath.combine sourceDirectory fileName

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

findCabalFile :: FilePath -> Action FilePath
findCabalFile filePath = do
  dict <- requestOf unBuildDict
  let packageName = takeBaseName $ takeDirectory filePath
  case Map.lookup (PackageName packageName) dict of
    Just x -> return x
    Nothing -> fail $ "Could not find source mapping for: " ++ filePath

findSourceDirectory :: FilePath -> Action FilePath
findSourceDirectory filePath =
  fmap takeDirectory $ findCabalFile filePath

class Cabal a where
  configAction   :: a -> FilePath -> GenericPackageDescription -> Action ()
  buildAction    :: a -> FilePath -> LocalBuildInfo -> Action ()
  copyAction     :: a -> FilePath -> LocalBuildInfo -> Action ()
  registerAction :: a -> FilePath -> LocalBuildInfo -> Action ()

-- |
-- If the build-type is Simple, try to drive the package build directly from this
-- process. cabal-install links a new setup program (which takes a while) and
-- calls out to that process to do each step.
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
               , configPackageDB = Setup.Flag buildDatabase
               , configUserInstall = Setup.Flag True}

          commonBuildInfo = emptyBuildInfo{hsSourceDirs = [sourceDir]}
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

-- |
-- For all other build-types just call out to cabal-install for the time being. When the dependency
-- on cabal-install is dropped these will need to be reimplemented via the package Setup.hs/lhs file
data CabalCustom = CabalCustom

instance Cabal CabalCustom where
  configAction _ filePath _ = do
    prefixDir  <- requestOf penvPrefixDirectory
    pkgConfDir <- requestOf penvPkgConfDirectory

    sourceDir <- findSourceDirectory filePath
    let args = ["configure", "-v0", "--prefix="++prefixDir, "--global", "--user", "--package-db="++pkgConfDir, "--builddir="++takeDirectory filePath]
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


class GetBuildType a where
  getBuildType :: a -> Maybe BuildType

instance GetBuildType LocalBuildInfo where
  getBuildType = buildType . localPkgDescr

instance GetBuildType GenericPackageDescription where
  getBuildType = buildType . packageDescription

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

cabalize :: Resource -> Rules ()
cabalize ghcPkgResource = do
  initializePackageConf
  cabalConfigure
  cabalBuild
  cabalCopy
  cabalRegister
  ghcPkgRegister ghcPkgResource

data ShakeMode
  = ShakeClean
      { desiredVerbosity :: Shake.Verbosity
      }
  | ShakeConfigure
      { desiredVerbosity :: Shake.Verbosity
      , desiredPrefix    :: FilePath
      }
  | ShakeBuild
      { desiredVerbosity :: Shake.Verbosity
      , desiredThreads   :: Maybe Int
      }
  | ShakeInstall
      { desiredVerbosity :: Shake.Verbosity
      }
    deriving (Show, Data, Typeable)

shakeMode :: ShakeMode
shakeMode = modes
  [ ShakeClean
     { desiredVerbosity = Shake.Normal &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     } &= name "clean"
  , ShakeConfigure
     { desiredVerbosity = Shake.Normal &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     , desiredPrefix    = "dist" </> "build" &= name "prefix" &= explicit &= help "Installation prefix"
     } &= name "configure"
  , ShakeBuild
     { desiredVerbosity = Shake.Normal &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     , desiredThreads   = Nothing &= name "j" &= name "jobs" &= explicit &= help "Number of parallel jobs"
     } &= name "build" &= auto
  , ShakeInstall
     { desiredVerbosity = Shake.Normal &= name "v" &= name "verbose" &= explicit &= help "Desired verbosity level"
     } &= name "install"
  ] &= program "shake"

findDirectoryBounds :: IO (FilePath, FilePath)
findDirectoryBounds = step1 =<< getCurrentDirectory where
  step1 dir = do
    exists <- Dir.doesFileExist (dir </> "Shakefile.hs")

    let parentDir = takeDirectory dir
    case exists of
      True                     -> step2 (dir, dir) parentDir 
      False | dir /= parentDir -> step1 parentDir
      _ -> fail "No Shakefile.hs found in any parent directories"

  step2 best@(_, top) dir = do
    fileFound <- Dir.doesFileExist (dir </> "Shakefile.hs")
    dbFound <- Dir.doesFileExist (dir </> ".shake.database")

    let parentDir = takeDirectory dir
    case (fileFound, dbFound) of
      (_, True)                    -> return (dir, top)
      (True, _) | dir /= parentDir -> step2 (dir, top) parentDir
                | otherwise        -> return (dir, top)
      _                            -> return best

data BuildTree
  = BuildAll
  | BuildChildren String
    deriving (Read, Show, Eq, Typeable)

instance Hashable BuildTree where
  hash BuildAll = hash (1 :: Int)
  hash (BuildChildren str) = hash (2 :: Int, str)

data BuildNode = BuildNode
  { buildFile      :: FilePath
  , buildChildren  :: [BuildNode]
  , buildSources   :: [String]
  , buildRegister  :: [FilePath]
  } deriving (Read, Show, Eq, Data, Typeable)

instance Hashable BuildNode where
  hash BuildNode{..} = hash (buildFile, buildChildren, buildSources)

instance NFData BuildTree where
  rnf BuildAll = ()
  rnf (BuildChildren x1) = ((rnf x1) `seq` ())

instance Binary BuildTree where
  put x
    = case x of
        BuildAll -> putWord8 0
        BuildChildren x1 -> do
          putWord8 1
          put x1

  get = do
    i <- getWord8;
    case i of
      0 -> return BuildAll
      1 -> fmap BuildChildren get
      _ -> error "Corrupted binary data for BuildTree"

instance NFData BuildNode where
  rnf (BuildNode x1 x2 x3 x4)
    = ((rnf x1)
       `seq` ((rnf x2) `seq` ((rnf x3) `seq` ((rnf x4) `seq` ()))))
instance Binary BuildNode where
  put (BuildNode x1 x2 x3 x4)
    = do { put x1;
           put x2;
           put x3;
           put x4 }
  get
    = do { x1 <- get;
           x2 <- get;
           x3 <- get;
           x4 <- get;
           return (BuildNode x1 x2 x3 x4) }


instance Rule BuildTree BuildNode where
  validStored _ _ = return True


generatePackageMap :: Request BuildDictionary -> Maybe (Action (Response BuildDictionary))
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

buildTree :: BuildTree -> Maybe (Action BuildNode)
buildTree (BuildChildren dir) = Just action where
  action = do
    rootDir <- requestOf penvRootDirectory
    buildDir <- requestOf penvBuildDirectory

    let shakefile = rootDir </> dir </> "Shakefile.hs"

    need [shakefile]
    (stdout, _) <- systemOutput "ghc" [shakefile, "-e", "print (children :: [String], sources :: [String])"]

    let (children, sources) = read stdout

    children' <- apply $ fmap (\ x -> BuildChildren $ dir </> x) children

    registrationFiles <- forM sources $ \ pkg -> do
      gdesc <- getPackageDescription $ dir </> pkg
      let packageName = display . pkgName . package . packageDescription $ gdesc
      return $! buildDir </> packageName </> "register"

    return $! BuildNode
      { buildFile     = rootDir </> dir
      , buildChildren = children'
      , buildSources  = sources
      , buildRegister = registrationFiles
      }


-- |
-- Each build type has a set of targets they want built
applyBuildActions :: ShakeMode -> (FilePath, FilePath) -> Action ()
applyBuildActions ShakeClean{} _ = do
  return ()

applyBuildActions ShakeConfigure{} _ = do
  _ <- apply1 (Request :: Request PersistedEnvironment)
  return ()

applyBuildActions ShakeBuild{} (_, currentDir) = do
  childNodes <- apply1 (BuildChildren currentDir)
  need [register | BuildNode{buildRegister} <- universe childNodes, register <- buildRegister]

applyBuildActions ShakeInstall{} _ = do
  return ()

main :: IO ()
main = do
  setDefaultUncaughtExceptionHandler
  -- "ghc --print-libdir" </> "package.conf.d" </> "package.cache"

  sm <- cmdArgs shakeMode

  dirs@(rootDir, _) <- findDirectoryBounds

  numProcs <- getNumProcessors
  ghcPkgResource <- newResource "ghc-pkg register" 1

  let threads = case sm of 
        ShakeBuild{desiredThreads = Just t} -> t
        _  -> numProcs

  shake shakeOptions{shakeFiles = rootDir </> ".shake", shakeThreads = threads, shakeVerbosity = desiredVerbosity sm} $ do
    rule (configureTheEnvironment dirs sm)
    rule buildTree
    rule generatePackageMap
    cabalize ghcPkgResource

    action $ do
      -- make sure this is not needed directly by any packages
      pkgConfDir <- requestOf penvPkgConfDirectory
      need [pkgConfDir </> "package.cache"]

      applyBuildActions sm dirs

