{-# LANGUAGE DeriveGeneric #-}

module DTP.Message where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )

data Message
  = Ping
  | Hello String
  | Fin
  | Ack deriving (Generic)

instance FromJSON Message
instance ToJSON Message
