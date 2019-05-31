{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.EchoReq 
  ( echoReq 
  ) where

import Protolude
import qualified Data.Aeson as DA
import Comm.Janus.Msgs.JSEP

data EchoReq = EchoReq
  { janus :: Text
  , body :: EchoReqBody
  , jsep :: JSEP
  , transaction :: Text
  , session_id :: Integer
  , handle_id :: Integer
  } deriving (Show,Generic)

data EchoReqBody = EchoReqBody
  { audio :: Bool
  , video :: Bool
  } deriving (Show,Generic)

instance DA.ToJSON EchoReq
instance DA.ToJSON EchoReqBody

echoReq :: Bool -> Bool -> JSEP -> Integer -> Integer -> Integer -> EchoReq
echoReq flAudio flVideo offerSDP sessID hndID transID =
    EchoReq
      { janus = "message"
      , body = EchoReqBody flAudio flVideo
      , transaction = show transID
      , session_id = sessID
      , handle_id = hndID
      , jsep = offerSDP
      }
