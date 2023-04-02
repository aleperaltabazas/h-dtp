module Network.TCP.Server
  ( bindTo
  , receive
  , listen
  )
where

import qualified Data.ByteString.Lazy          as BL
import           Data.Int
import           Network.Socket
import           Network.Socket.ByteString.Lazy
import           Network.TCP.Internal

bindTo :: Int -> IO Socket
bindTo port = withSocketsDo $ do
  addr <- resolveAddress Nothing port
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  withFdSocket sock setCloseOnExecIfNeeded
  bind sock $ addrAddress addr
  return sock

receive :: Int64 -> Socket -> IO BL.ByteString
receive = flip recv
