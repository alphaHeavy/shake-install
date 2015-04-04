{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main (main) where

import Data.Generics.Uniplate.DataOnly
import Development.Shake as Shake
import Development.Shake.Rule
import Development.Shake.Install.BuildTree as Shake
import Development.Shake.Install.Exceptions as Shake
import Development.Shake.Install.PersistedEnvironment as Shake
import Development.Shake.Install.RequestResponse as Shake
import Development.Shake.Install.Rules as Shake
import Development.Shake.Install.ShakeMode as Shake
import Development.Shake.Install.Utils as Shake
import GHC.Conc (getNumProcessors)
import System.Exit (exitWith)
import System.FilePath
import System.Process (rawSystem)
import Options.Applicative (execParser)
import Control.Exception.Base (throwIO)

-- |
-- Each build type has a set of targets they want built
applyBuildActions :: ShakeMode -> (FilePath, FilePath) -> Action ()
applyBuildActions ShakeClean{} _ =
  return ()

applyBuildActions ShakeConfigure{} _ = do
  x <- apply1 (Request :: Request PersistedEnvironment)
  let _ = (x :: Response PersistedEnvironment)
  return ()

applyBuildActions ShakeBuild{} (_, currentDir) = do
  childNodes <- apply1 (BuildChildren currentDir)
  need [register | BuildNode{buildRegister} <- universe childNodes, register <- buildRegister]

applyBuildActions ShakeInstall{} _ =
  return ()

applyBuildActions ShakeGhci{} _ =
  liftIO $ throwIO $ userError "impossible! did not expect ShakeGhci"

classifyBuildStyle
  :: ShakeMode
  -> BuildStyle
classifyBuildStyle sm = case sm of
  ShakeBuild BuildOpts{desiredRecurse = True} ->
    BuildRecursiveWildcard
      {
      }

  sb@(ShakeBuild BuildOpts{desiredPackages, desiredRoots}) | explicitPaths sb ->
    BuildWithExplicitPaths
      { buildCabalFiles = desiredPackages
      , buildDepends    = desiredRoots
      }

  _ ->
    BuildViaShakefile
      {
      }

 where
  explicitPaths (ShakeBuild BuildOpts{desiredPackages, desiredRoots})
    | not . null $ desiredPackages = True
    | not . null $ desiredRoots    = True
  explicitPaths _                  = False

main :: IO ()
main = do
  setDefaultUncaughtExceptionHandler
  -- "ghc --print-libdir" </> "package.conf.d" </> "package.cache"

  so <- execParser programParser

  -- need to scan directories to locate the .shake.database
  dirs@(rootDir, _) <- findDirectoryBounds

  numProcs <- getNumProcessors
  ghcPkgResource <- newResourceIO "ghc-pkg register" 1
  hintResource <- newResourceIO "hint interpreter" 1

  let sm = shakeMode so
  let threads = case sm of
        ShakeBuild BuildOpts{buildThreads = Just t} -> t
        _  -> numProcs

      options = shakeOptions
       { shakeFiles     = rootDir </> ".shake"
       , shakeThreads   = threads
       , shakeVerbosity = desiredVerbosity so
       , shakeStaunch   = desiredStaunch sm }

      style = classifyBuildStyle sm

  shake options $ do
    rule (configureTheEnvironment dirs sm)
    rule (buildTree hintResource style)
    rule generatePackageMap
    initializePackageConf
    initializeProgramDb
    cabalConfigure
    cabalBuild
    cabalCopy
    cabalRegister
    ghcPkgRegister ghcPkgResource

    action $ do
      -- make sure this is not needed directly by any packages
      pkgConfDir <- requestOf penvPkgConfDirectory
      need [pkgConfDir </> "package.cache", pkgConfDir </> "program.db"]

      case sm of
        ShakeGhci GhciOpts{desiredArgs} -> liftIO $ do
          let desiredArgs' = "-package-conf=/Users/steve/source/eng/build/package.conf.d" : desiredArgs
          ec <- rawSystem "ghci" desiredArgs'
          exitWith ec

        _ -> 
          applyBuildActions sm dirs

