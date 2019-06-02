{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

module Comm.Janus.Msgs.VideoCallCallReq where

import Protolude
import qualified Data.Aeson as DA
import Comm.Janus.Msgs.JanusReq

data VideoCallCallReqBody = VideoCallCallReqBody
  { request :: Text -- call
  , username :: Text
  } deriving (Show,Generic)

instance DA.ToJSON VideoCallCallReqBody

newtype VideoCallCallReqPs =  VideoCallCallReqPs 
  { userName :: Text
  } deriving (Show,Eq)

instance JanusBodyBuilder VideoCallCallReqPs VideoCallCallReqBody 'JSepTrue where
  createReqBody ps = 
    VideoCallCallReqBody
      { request = "call"
      , username = userName ps
      } 

