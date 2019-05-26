{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.RegisterReq where

import Protolude
import qualified Data.Aeson as DA

{-
{
  "janus": "message",
  "body": {
    "request": "register",
    "username": "sip:112229_10006@toronto2.voip.ms",
    "display_name": "Boris",
    "secret": "Boris100",
    "proxy": "sip:@toronto2.voip.ms"
  },
  "transaction": "poYo3it4lYtl",
  "session_id": 6322890423209415,
  "handle_id": 649781573617088
}
-}

data RegisterReqBody = RegisterReqBody
  { request :: Text -- register
  , username :: Text
  , display_name :: Text
  , secret :: Text
  , proxy :: Text
  } deriving (Show,Generic)

data RegisterReq = RegisterReq
  { janus :: Text
  , body :: RegisterReqBody
  , transaction :: Text
  , session_id :: Integer
  , handle_id :: Integer
  } deriving (Show,Generic)

instance DA.ToJSON RegisterReqBody
instance DA.ToJSON RegisterReq

registerReq :: Text -> Text -> Text -> Text -> Integer -> Integer -> Integer -> RegisterReq
registerReq userName displayName pwd prxy sessID hndID transID =
  let regBody = RegisterReqBody
                  { request = "register"
                  , username = userName
                  , display_name = displayName
                  , secret = pwd
                  , proxy = prxy
                  }
  in RegisterReq
    { janus = "message"
    , transaction = show transID
    , session_id = sessID
    , handle_id = hndID
    , body = regBody
    }

-- "janus":"attach","plugin":"janus.plugin.sip","opaque_id":"siptest-mXBwVOTlDmM7","transaction":"71eiU1CwLX4Y","force-bundle":true,"force-rtcp-mux":true,"session_id":6322890423209415
