module REPL.Commands.Exit where

import           Data.IORef
import           DTP
import           REPL.Types
import           System.Exit                    ( exitSuccess
                                                , exitFailure
                                                )

exit :: REPL ()
exit = do
  ref <- readRemoteRef
  case ref of
    Just remote -> liftIO $ do
      -- TODO: disconnect from the remote
      putStrLn "Bye!"
      exitSuccess
    Nothing -> liftIO $ do
      putStrLn "Bye!"
      exitSuccess
