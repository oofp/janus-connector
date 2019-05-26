{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.AcceptReq 
  ( acceptReq 
  ) where

import Protolude
import qualified Data.Aeson as DA
import Comm.Janus.Msgs.JSEP

data AcceptReq = AcceptReq
  { janus :: Text
  , body :: AcceptReqBody
  , jsep :: JSEP
  , transaction :: Text
  , session_id :: Integer
  , handle_id :: Integer
  } deriving (Show,Generic)

data AcceptReqBody = AcceptReqBody
  { request :: Text -- accept
  } deriving (Show,Generic)

instance DA.ToJSON AcceptReq
instance DA.ToJSON AcceptReqBody


acceptReq :: JSEP -> Integer -> Integer -> Integer -> AcceptReq
acceptReq answerSDP sessID hndID transID =
    AcceptReq
      { janus = "message"
      , body = AcceptReqBody "accept"
      , transaction = show transID
      , session_id = sessID
      , handle_id = hndID
      , jsep = answerSDP
      }
