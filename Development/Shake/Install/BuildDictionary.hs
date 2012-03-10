{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Development.Shake.Install.BuildDictionary where

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

