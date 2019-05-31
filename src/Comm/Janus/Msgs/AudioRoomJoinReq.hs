{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.AudioRoomJoinReq where

import Protolude
import qualified Data.Aeson as DA



-- {"janus":"message","body":{"request":"join","room":1234,"display":"User1"},"transaction":"4RibXTAidOzQ"}

data AudioRoomJoinReqBody = AudioRoomJoinReqBody
  { request :: Text -- join
  , room :: Int
  , display :: Text
  } deriving (Show,Generic)

data AudioRoomJoinReq = AudioRoomJoinReq
  { janus :: Text
  , body :: AudioRoomJoinReqBody
  , transaction :: Text
  , session_id :: Integer
  , handle_id :: Integer
  } deriving (Show,Generic)

instance DA.ToJSON AudioRoomJoinReqBody
instance DA.ToJSON AudioRoomJoinReq

audioRoomJoinReq :: Int -> Text -> Integer -> Integer -> Integer -> AudioRoomJoinReq
audioRoomJoinReq roomNum displayTxt sessID hndID transID =
  let reqBody = AudioRoomJoinReqBody
                  { request = "join"
                  , room = roomNum
                  , display = displayTxt
                  }
  in AudioRoomJoinReq
    { janus = "message"
    , transaction = show transID
    , session_id = sessID
    , handle_id = hndID
    , body = reqBody
    }
