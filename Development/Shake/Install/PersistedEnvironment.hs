{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Shake.Install.PersistedEnvironment where

import Control.Applicative
import Control.DeepSeq
import Control.Monad (liftM5)
import Data.Binary
import Data.Hashable
import Data.Typeable
import Development.Shake as Shake
import qualified System.Directory as Dir

newtype PersistedEnvironmentRequest = PersistedEnvironmentRequest ()
  deriving (Eq, Show, Binary, NFData, Hashable, Typeable)

data PersistedEnvironment = PersistedEnvironment
  { penvEnvironment      :: [(String, String)]
  , penvRootDirectory    :: FilePath
  , penvBuildDirectory   :: FilePath
  , penvPrefixDirectory  :: FilePath
  , penvPkgConfDirectory :: FilePath
  } deriving (Show, Ord, Typeable)

instance Eq PersistedEnvironment where
  _ == _ = True

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
  hashWithSalt s PersistedEnvironment{..} = hashWithSalt s
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

instance Rule PersistedEnvironmentRequest PersistedEnvironment where
  storedValue _ = do
    let fp = "/tmp/env.bin"
    exists <- Dir.doesFileExist fp
    if exists
      then Just <$> decodeFile fp
      else return Nothing
