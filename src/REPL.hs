module REPL
  ( module REPL.Console
  , repl
  )
where

import           Control.Monad

import           Control.Monad.Trans.Reader
import           Data.IORef
import           Data.List.Split
import           DTP
import           REPL.Console
import           REPL.Commands.Connect
import           REPL.Types
import           System.Exit

repl :: REPL ()
repl = forever $ do
  remoteRef <- ask
  remote    <- liftIO $ readIORef remoteRef
  command   <- liftIO $ case remote of
    Nothing -> prompt "> "
    Just r  -> prompt $ name r ++ "> "
  runCommand (words command)

runCommand :: [String] -> REPL ()
runCommand []               = return ()
runCommand ("connect" : xs) = case xs of
  [_] -> liftIO $ do
    putStrLn "Error: missing port"
    putStrLn "Usage: connect host port"
  [host, port] -> connect "falopa" host port
  _            -> liftIO $ do
    putStrLn "Too many arguments"
    putStrLn "Usage: connect host port"
runCommand ("exit" : _) = liftIO $ do
  newline
  putStrLn "Bye!"
  exitSuccess
runCommand s = liftIO $ putStrLn $ "Unknown command " ++ unwords s
