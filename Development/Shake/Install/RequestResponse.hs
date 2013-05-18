{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Development.Shake.Install.RequestResponse where

import Control.Applicative
import Development.Shake as Shake

import Data.Data
import Data.Binary
import Control.DeepSeq
import Data.Hashable

{-

newtype Request a = Request FilePath
  deriving (Show, Typeable, NFData, Eq, Hashable, Binary)

data Response a = Response{unResponse :: a}

deriving instance Show a => Show (Response a)
deriving instance Read a => Read (Response a)
deriving instance Eq a => Eq (Response a)
deriving instance Ord a => Ord (Response a)
deriving instance Typeable1 Response

instance Hashable a => Hashable (Response a) where
  hashWithSalt s (Response x) = hashWithSalt s x

instance Binary a => Binary (Response a) where
  get = Response <$> get
  put (Response x) = put x

instance NFData a => NFData (Response a) where
  rnf (Response x) = rnf x `seq` ()

instance (ShakeValue b, ShakeValue a, a ~ b) => Rule (Request a) (Response b) where
  storedValue _ = return Nothing

requestOf
  :: forall a b . Typeable a => Rule (Request a) (Response a)
  => (a -> b) -- ^ record field selector
  -> Action b
requestOf fun =
  fun . unResponse <$> askOracle (Request (show (typeOf (undefined :: a))) :: Request a)
-}
