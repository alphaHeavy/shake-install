{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Shake.Install.PersistedEnvironment where

import Control.DeepSeq
import Control.Monad (liftM5)
import Data.Binary
import Data.Hashable
import Data.Typeable

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
    liftM5
      PersistedEnvironment
        get
        get
        get
        get
        get

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

