{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.CreateConReq where

import Protolude
import qualified Data.Aeson as DA

data CreateConReq = CreateConReq
  { janus :: Text
  , transaction :: Text
  } deriving (Generic)

instance DA.ToJSON CreateConReq
