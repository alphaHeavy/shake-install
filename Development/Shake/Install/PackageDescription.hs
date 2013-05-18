{-# LANGUAGE TemplateHaskell #-}

module Development.Shake.Install.PackageDescription where

-- import Control.Applicative
import Control.Lens
-- import Distribution.Package
import Distribution.PackageDescription -- (package, packageDescription, emptyBuildInfo, hsSourceDirs)

makeClassy ''Benchmark
makeClassy ''BenchmarkInterface
makeClassy ''BenchmarkType
makeClassy ''BuildInfo
makeClassy ''ConfVar
makeLenses ''Condition
makeClassy ''Executable
makeClassy ''Flag
makeClassy ''FlagName
makeClassy ''GenericPackageDescription
makeClassy ''Library
makeClassy ''PackageDescription
makeClassy ''RepoKind
makeClassy ''RepoType
makeClassy ''SourceRepo
makeClassy ''TestSuite
makeClassy ''TestSuiteInterface

{-
condTreeData :: Lens' (CondTree v c a) a
condTreeData = lens g s where
  g (CondNode a _ _) = a
  s (CondNode _ c v) a = CondNode a c v

condTreeConstraints :: Lens' (CondTree v c a) c
condTreeConstraints = lens g s where
  g (CondNode _ c _) = c
  s (CondNode a _ v) c = CondNode a c v

condTreeComponents :: Traversal' (CondTree v c a) (Condition v, CondTree v c a, Maybe (CondTree v c a))
condTreeComponents = components.each where
  components = lens g s
  g (CondNode _ _ v) = v
  s (CondNode a c _) = CondNode a c
-}

treeData :: Lens' (CondTree a c v) v
treeData = lens condTreeData (\ ct f -> ct{condTreeData = f})

libraryBuildInfo :: Traversal' GenericPackageDescription BuildInfo
libraryBuildInfo = lib.treeData.bi where
  bi = lens libBuildInfo (\ l lbi -> l{libBuildInfo = lbi})
  lib = lens condLibrary (\ gpd f -> gpd{condLibrary = f}) . each

executableBuildInfos :: Traversal' GenericPackageDescription BuildInfo
executableBuildInfos = exe.treeData.bi where
  bi = lens Distribution.PackageDescription.buildInfo (\ l ebi -> l{Distribution.PackageDescription.buildInfo = ebi})
  exe = lens condExecutables (\ gpd f -> gpd{condExecutables = f}) . traverse . traverseOf _2

testSuiteBuildInfos :: Traversal' GenericPackageDescription BuildInfo
testSuiteBuildInfos = test.treeData.bi where
  bi = lens Distribution.PackageDescription.testBuildInfo (\ l ebi -> l{Distribution.PackageDescription.testBuildInfo = ebi})
  test = lens condTestSuites (\ gpd f -> gpd{condTestSuites = f}) . traverse . traverseOf _2

benchmarkBuildInfos :: Traversal' GenericPackageDescription BuildInfo
benchmarkBuildInfos = bm.treeData.bi where
  bi = lens Distribution.PackageDescription.benchmarkBuildInfo (\ l ebi -> l{Distribution.PackageDescription.benchmarkBuildInfo = ebi})
  bm = lens condBenchmarks (\ gpd f -> gpd{condBenchmarks = f}) . traverse . traverseOf _2

_hsSourceDirs :: Traversal' BuildInfo FilePath
_hsSourceDirs = lens hsSourceDirs (\ bi dirs -> bi{hsSourceDirs = dirs}) . each

_cSources :: Traversal' BuildInfo FilePath
_cSources = lens cSources (\ bi f -> bi{cSources = f}) . each
