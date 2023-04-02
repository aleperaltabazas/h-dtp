module REPL.Commands.Connect where

import           Data.IORef
import qualified DTP
import           REPL.Types

connect :: String -> String -> String -> REPL ()
connect ownId host port = do
  ref <- ask
  e   <- liftIO $ DTP.handshake ownId host (read port)
  case e of
    Left  err -> liftIO $ putStrLn err
    Right r   -> liftIO $ writeIORef ref (Just r)
