{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

module Comm.Janus.Msgs.VideoCallRegisterReq where

import Protolude
import qualified Data.Aeson as DA
import Comm.Janus.Msgs.JanusReq

{-
janus: "message", body: {…}, transaction: "kBEr3zqqoM8v"}
body: {request: "register", username: "b1"}
handle_id: 7728127179290039
janus: "message"
session_id: 7713547946485051
transaction: "kBEr3zqqoM8v"
-}

data VideoCallRegisterReqBody = VideoCallRegisterReqBody
  { request :: Text -- regiter
  , username :: Text
  } deriving (Show,Generic)

instance DA.ToJSON VideoCallRegisterReqBody

newtype VideoCallRegisterReqPs =  VideoCallRegisterReqPs 
  { userName :: Text
  } deriving (Show,Eq)

instance JanusBodyBuilder VideoCallRegisterReqPs VideoCallRegisterReqBody 'JSepFalse where
  createReqBody ps = 
    VideoCallRegisterReqBody
      { request = "register"
      , username = userName ps
      } 

{-
data VideoCallRegisterReq = VideoCallRegisterReq
  { janus :: Text
  , body :: VideoCallRegisterReqBody
  , transaction :: Text
  , session_id :: Integer
  , handle_id :: Integer
  } deriving (Show,Generic)

instance DA.ToJSON VideoCallRegisterReq

videoCallRegisterReq :: Text -> Integer -> Integer -> Integer -> VideoCallRegisterReq
videoCallRegisterReq userNameTxt sessID hndID transID =
  let reqBody = VideoCallRegisterReqBody
                  { request = "register"
                  , username = userNameTxt
                  }
  in VideoCallRegisterReq
    { janus = "message"
    , transaction = show transID
    , session_id = sessID
    , handle_id = hndID
    , body = reqBody
    }
-}
