{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Development.Shake.Install.RequestResponse where

import Development.Shake as Shake

import Control.Applicative
import Data.Data
import Data.Binary
import Control.DeepSeq
import Data.Hashable


data Request a = Request
  deriving (Show)

instance Typeable1 Request where
  typeOf1 _ = mkTyConApp (mkTyCon3 "shake" "Main" "Request") []

instance Typeable a => Typeable (Request a) where
  typeOf = typeOfDefault

instance NFData (Request a) where
  rnf _ = ()

instance Eq (Request a) where
  _ == _ = True

instance Hashable (Request a) where
  hashWithSalt _ _ = 1

instance Binary (Request a) where
  get = return Request
  put _ = return ()


data Response a = Response{unResponse :: a}

deriving instance Show a => Show (Response a)
deriving instance Read a => Read (Response a)
deriving instance Eq a => Eq (Response a)
deriving instance Ord a => Ord (Response a)
deriving instance Typeable1 Response

instance Hashable a => Hashable (Response a) where
  hashWithSalt s (Response x) = hashWithSalt s x

instance Binary a => Binary (Response a) where
  get = fmap Response get
  put (Response x) = put x

instance NFData a => NFData (Response a) where
  rnf (Response x) = rnf x `seq` ()

instance (Show a, Binary a, NFData a, Typeable a, Hashable a, Eq a)
  => Rule (Request a) (Response a) where
  storedValue _ = return Nothing

requestOf
  :: forall a b . Rule (Request a) (Response a)
  => (a -> b) -- ^ record field selector
  -> Action b
requestOf fun =
  fun . unResponse <$> apply1 (Request :: Request a)
