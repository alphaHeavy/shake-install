{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Development.Shake.Install.BuildDictionary
  ( BuildDictionary(..)
  , findCabalFile
  , findSourceDirectory
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Data
import Data.Hashable
import Data.Map as Map
import Distribution.Package
import Distribution.Text (display)
import System.FilePath

import Development.Shake (Action)
import Development.Shake.Install.RequestResponse (requestOf)

instance Hashable (Map PackageName FilePath) where
  hashWithSalt s = hashWithSalt s . toList

instance Hashable PackageName where
  hashWithSalt s = hashWithSalt s . display

newtype BuildDictionary = BuildDictionary{unBuildDict :: Map PackageName FilePath}
  deriving (Read, Show, Eq, Typeable, NFData, Binary, Hashable)

findCabalFile
  :: FilePath
  -> Action FilePath
findCabalFile filePath = do
  dict <- requestOf unBuildDict
  let packageName = takeBaseName $ takeDirectory filePath
  case Map.lookup (PackageName packageName) dict of
    Just x -> return x
    Nothing -> fail $ "Could not find source mapping for: " ++ filePath

findSourceDirectory
  :: FilePath
  -> Action FilePath
findSourceDirectory filePath =
  fmap takeDirectory $ findCabalFile filePath

