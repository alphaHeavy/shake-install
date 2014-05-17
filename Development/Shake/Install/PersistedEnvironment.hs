{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Shake.Install.PersistedEnvironment where

import Control.DeepSeq
import Control.Monad (liftM5)
import Data.Binary
import Data.Hashable
import Data.Typeable

data PersistedEnvironment = PersistedEnvironment
  { penvRootDirectory    :: FilePath
  , penvBuildDirectory   :: FilePath
  , penvPrefixDirectory  :: FilePath
  , penvPkgConfDirectory :: FilePath
  , penvAdditionalPkgConfDirectories :: [FilePath]
  } deriving (Show, Eq, Ord, Typeable)

instance Binary PersistedEnvironment where
  put PersistedEnvironment{..} = do
    put penvRootDirectory
    put penvBuildDirectory
    put penvPrefixDirectory
    put penvPkgConfDirectory
    put penvAdditionalPkgConfDirectories

  get =
    liftM5
      PersistedEnvironment
        get
        get
        get
        get
        get

instance Hashable PersistedEnvironment where
  hashWithSalt s PersistedEnvironment{..} = hashWithSalt s
    ( penvRootDirectory
    , penvBuildDirectory
    , penvPrefixDirectory
    , penvPkgConfDirectory
    , penvAdditionalPkgConfDirectories)

instance NFData PersistedEnvironment where
  rnf PersistedEnvironment{..} =
    rnf penvRootDirectory `seq`
    rnf penvBuildDirectory `seq`
    rnf penvPrefixDirectory `seq`
    rnf penvPkgConfDirectory `seq`
    rnf penvAdditionalPkgConfDirectories `seq`
    ()
