module REPL.Types
  ( module Control.Monad.Trans.Reader
  , liftIO
  , REPL
  , readRemoteRef
  )
where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import           DTP

type REPL = ReaderT (IORef (Maybe Remote)) IO

readRemoteRef :: REPL (Maybe Remote)
readRemoteRef = do
  ref <- ask
  liftIO $ readIORef ref
