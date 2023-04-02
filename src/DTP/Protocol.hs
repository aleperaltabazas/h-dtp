{-# LANGUAGE RecordWildCards #-}

module DTP.Protocol
  ( send
  , receive
  , start
  , handshake
  , awaitHandshake
  )
where

import           Data.Aeson
import qualified Data.Binary                   as Binary
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8
import qualified Data.ByteString.Lazy          as BL
import           Data.Int                       ( Int64 )
import           DTP.Message
import           DTP.Remote
import qualified Network.TCP                   as TCP

send :: Message -> Remote -> IO ()
send m remote = sendBufferWithSize (encode m) (socket remote)

receive :: Remote -> IO (Maybe Message)
receive = receiveMessage . socket

start :: String -> Int -> IO Remote
start name port = do
  socket <- TCP.bindTo port
  TCP.listen socket 1
  return Remote { .. }

handshake :: String -> String -> Int -> IO (Either String Remote)
handshake name host port = do
  socket <- TCP.connect host port
  sendBufferWithSize (encode $ Hello name) socket
  helloBack <- receiveMessage socket
  case helloBack of
    Just (Hello name) -> return $ Right Remote { .. }
    Just _            -> return $ Left "Unexpected answer from remote"
    Nothing           -> return $ Left "An unexpected error occurred"

awaitHandshake :: Remote -> String -> IO (Either String Remote)
awaitHandshake remote name = do
  (conn, _) <- TCP.accept (socket remote)
  hello     <- receiveMessage conn
  case hello of
  -- TODO: add passphrase and stuff
    Just (Hello clientName) -> do
      sendBufferWithSize (encode $ Hello "falopa") conn
      return $ Right Remote { name = clientName, socket = conn }
    _ -> return $ Left "Unexpected error"

sendBufferWithSize :: BL.ByteString -> TCP.Socket -> IO ()
sendBufferWithSize bytes socket = do
  let len  = BL.length bytes
  let size = Binary.encode len
  let buf  = size <> bytes
  TCP.send socket buf

receiveMessage :: TCP.Socket -> IO (Maybe Message)
receiveMessage socket = do
  sizeBuf <- TCP.receive int64Size socket
  let size = Binary.decode sizeBuf :: Int64
  buf <- TCP.receive size socket
  return $ decode buf
  where int64Size = 8
