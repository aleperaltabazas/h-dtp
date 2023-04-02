{-# LANGUAGE MultiWayIf #-}

module Main
  ( main
  )
where

import           Control.Monad
import           Control.Monad.Trans.Reader
import           Control.Concurrent
import           Data.IORef
import           DTP
import qualified Network.TCP                   as TCP
import           REPL
import           System.Environment
import           System.Exit

main :: IO ()
main = do
  args      <- getArgs
  remoteRef <- newIORef Nothing
  if
    | null args -> do
      putStrLn "Tell me the port!"
      exitFailure
    | length args == 1 -> do
      server <- start "falopa" (read . head $ args)
      forkIO $ acceptConnections remoteRef server "falopa"
      putStrLn $ "Bound to port " ++ head args
    | otherwise -> do
      server <- start "falopa" (read . head $ args)
      forkIO $ acceptConnections remoteRef server "falopa"
      putStrLn $ "Bound to port " ++ head args
      eRemote <- handshake "faloita" "localhost" (read $ args !! 1)
      case eRemote of
        Left err -> do
          putStrLn err
          exitFailure
        Right remote -> do
          putStrLn "Connected"
  runReaderT repl remoteRef

acceptConnections :: IORef (Maybe Remote) -> Remote -> String -> IO ()
acceptConnections remoteRef self n = forever $ do
  rem <- awaitHandshake self n
  case rem of
    Left  err -> putStrLn err
    Right r   -> do
      newline
      putStrLn $ "Connected to " ++ name r
      newline
      prompt $ name r ++ "> "
      writeIORef remoteRef (Just r)
