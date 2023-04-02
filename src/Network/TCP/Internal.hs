module Network.TCP.Internal
  ( resolveAddress
  )
where

import           Network.Socket
import           Network.Socket.ByteString

resolveAddress :: Maybe String -> Int -> IO AddrInfo
resolveAddress host port = do
  let hints =
        defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  head <$> getAddrInfo (Just hints) host (Just $ show port)
