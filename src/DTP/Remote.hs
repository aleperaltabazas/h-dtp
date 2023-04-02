module DTP.Remote where

import           Network.TCP                    ( Socket )
import qualified Network.TCP                   as TCP

data Remote
  = Remote
  { socket :: Socket
  , name :: String
  }
