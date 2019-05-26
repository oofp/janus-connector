{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.TrickleConnectedReq where

import Protolude
import qualified Data.Aeson as DA

{-
{"janus":"trickle","candidate":{"completed":true},"transaction":"NhBtRC8qD8LX","session_id":6322890423209415,"handle_id":649781573617088}
-}
newtype IceCompletedFlag = IceCompletedFlag
  { completed :: Bool
  } deriving (Show,Generic)

data TrickleConnectedReq = TrickleConnectedReq
  { janus :: Text -- "janus": "trickle"
  , candidate :: IceCompletedFlag
  , transaction :: Text
  , session_id :: Integer
  , handle_id :: Integer
  } deriving (Show,Generic)

instance DA.ToJSON IceCompletedFlag
instance DA.ToJSON TrickleConnectedReq

trickleConnectedReq :: Integer -> Integer -> Integer -> TrickleConnectedReq
trickleConnectedReq sessID hndID transID =
  TrickleConnectedReq
    { janus = "trickle"
    , transaction = show transID
    , session_id = sessID
    , handle_id = hndID
    , candidate = IceCompletedFlag True
    }
