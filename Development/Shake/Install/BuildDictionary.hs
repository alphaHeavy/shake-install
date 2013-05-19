{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Development.Shake.Install.BuildDictionary
  ( BuildDictionary(..)
  , findCabalFile
  , findSourceDirectory
  ) where

import Control.Applicative ((<$>))
import Control.DeepSeq
import Data.Binary
import Data.Data
import Data.Hashable
import Data.Map as Map
import Development.Shake (Rule(..), apply1)
import Distribution.InstalledPackageInfo.Binary ()
import Distribution.Package
import Distribution.Text (display)
import qualified System.Directory as Dir
import System.FilePath

import Development.Shake (Action)
-- import Development.Shake.Install.RequestResponse (requestOf)

instance NFData PackageIdentifier where
  rnf (PackageIdentifier x1 x2)
    = rnf x1 `seq` rnf x2 `seq` ()

instance NFData PackageName where
  rnf (PackageName x1) = rnf x1 `seq` ()

instance Hashable (Map PackageName FilePath) where
  hashWithSalt s = hashWithSalt s . toList

instance Hashable PackageName where
  hashWithSalt s = hashWithSalt s . display

newtype BuildDictionary = BuildDictionary ()
  deriving (Read, Show, Eq, Typeable, NFData, Binary, Hashable)

deriving instance Typeable PackageName

{-
cabalBlah :: Rules ()
cabalBlah = do
  addOracle $ \(
  -}

findCabalFile
  :: FilePath
  -> Action FilePath
findCabalFile filePath = do
  dict <- apply1 (BuildDictionary ())
  let packageName = takeBaseName $ takeDirectory filePath
  case Map.lookup (PackageName packageName) dict of
    Just x -> return x
    Nothing -> fail $ "Could not find source mapping for: " ++ filePath

findSourceDirectory
  :: FilePath
  -> Action FilePath
findSourceDirectory filePath =
  fmap takeDirectory $ findCabalFile filePath

instance Rule BuildDictionary (Map PackageName FilePath) where
  storedValue _ = do
    let fp = "/tmp/dict.bin"
    exists <- Dir.doesFileExist fp
    if exists
      then Just <$> decodeFile fp
      else return Nothing
