{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Development.Shake.Install.Exceptions where

import Control.Exception (Exception(..), SomeException(..))
import Data.IORef (IORef, newIORef, writeIORef)
import Data.Typeable (Typeable)
import GHC.Conc (setUncaughtExceptionHandler)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Signals (raiseSignal, sigABRT)

-- | a dummy exception to initialize the global IORef with
data NoException = NoException deriving (Typeable)

instance Show NoException where
  show _ = "<<no exception>>"

instance Exception NoException

-- | storage for the last exception
lastException :: IORef SomeException
{-# NOINLINE lastException #-}
lastException = unsafePerformIO . newIORef $ SomeException NoException

-- | when no catch frame handles an exception dump core and terminate the process
uncaughtExceptionHandler :: SomeException -> IO ()
{-# NOINLINE uncaughtExceptionHandler #-}
uncaughtExceptionHandler !e = do
  writeIORef lastException e
  hPutStrLn stderr $ "Unhandled exception: " ++ show e
  raiseSignal sigABRT

setDefaultUncaughtExceptionHandler :: IO ()
setDefaultUncaughtExceptionHandler =
  setUncaughtExceptionHandler uncaughtExceptionHandler

