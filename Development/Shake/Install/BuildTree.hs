{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Development.Shake.Install.BuildTree
  ( BuildTree(..)
  , BuildNode(..)
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Exception (SomeException, try)
import Data.Binary
import Data.Data
import Data.Hashable
import Development.Shake as Shake
import Development.Shake.Rule
import System.FilePath
import System.IO

data BuildTree
  = BuildChildren String
    deriving (Read, Show, Eq, Typeable)

instance Hashable BuildTree where
  hashWithSalt s (BuildChildren str) = hashWithSalt s (2 :: Int, str)

instance NFData BuildTree where
  rnf (BuildChildren x1) = rnf x1 `seq` ()

instance Binary BuildTree where
  put x
    = case x of
        BuildChildren x1 -> do
          putWord8 1
          put x1

  get = do
    i <- getWord8;
    case i of
      1 -> fmap BuildChildren get
      _ -> error "Corrupted binary data for BuildTree"

data BuildNode = BuildNode
  { buildFile      :: FilePath
  , buildChildren  :: [BuildNode]
  , buildSources   :: [String]
  , buildRegister  :: [FilePath]
  } deriving (Read, Show, Eq, Data, Typeable)

instance Hashable BuildNode where
  hashWithSalt s BuildNode{..} = hashWithSalt s (buildFile, buildChildren, buildSources)

instance NFData BuildNode where
  rnf (BuildNode x1 x2 x3 x4)
    = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` ()

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
  storedValue _ (BuildChildren path) = do
    mval <- try $ withFile (path </> ".shake.children") ReadMode $ \ h -> do
      children <- read <$> hGetContents h
      return $!! children

    return $ either (const Nothing) Just (mval :: Either SomeException BuildNode)
