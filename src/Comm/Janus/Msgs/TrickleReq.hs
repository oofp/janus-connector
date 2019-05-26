{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.TrickleReq where

import Protolude
import qualified Data.Aeson as DA
import qualified Comm.Janus.Msgs.Candidate as Cand

{-
{
  "janus": "trickle",
  "candidate": {
    "candidate": "candidate:2999745851 1 udp 2122260223 192.168.56.1 53589 typ host generation 0 ufrag Shll network-id 1",
    "sdpMid": "audio",
    "sdpMLineIndex": 0
  },
  "transaction": "318oURVhlBlN",
  "session_id": 6322890423209415,
  "handle_id": 649781573617088
}
-}

data TrickleReq = TrickleReq
  { janus :: Text -- "janus": "trickle"
  , candidate :: Cand.Candidate
  , transaction :: Text
  , session_id :: Integer
  , handle_id :: Integer
  } deriving (Show,Generic)

instance DA.ToJSON TrickleReq

trickleReq :: Cand.Candidate -> Integer -> Integer -> Integer -> TrickleReq
trickleReq cand sessID hndID transID =
  TrickleReq
    { janus = "trickle"
    , transaction = show transID
    , session_id = sessID
    , handle_id = hndID
    , candidate = cand
    }
