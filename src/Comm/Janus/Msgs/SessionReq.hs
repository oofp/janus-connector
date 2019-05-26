{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.SessionReq where

import Protolude
import qualified Data.Aeson as DA

data SessionReq = SessionReq
  { janus :: Text
  , transaction :: Text
  , session_id :: Integer
  } deriving (Show, Generic)

instance DA.ToJSON SessionReq
