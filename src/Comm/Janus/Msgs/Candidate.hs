{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.Candidate where

import Protolude
import qualified Data.Aeson as DA
import qualified Data.ByteString.Lazy as BL

{-
{
    "candidate": "candidate:2999745851 1 udp 2122260223 192.168.56.1 53589 typ host generation 0 ufrag Shll network-id 1",
    "sdpMid": "audio",
    "sdpMLineIndex": 0
  }
-}

data Candidate = Candidate
  { candidate :: Text
  , sdpMid :: Text
  , sdpMLineIndex :: Int
  } deriving (Show,Generic)

instance DA.ToJSON Candidate
instance DA.FromJSON Candidate

decodeCandidate :: Text -> Maybe Candidate
decodeCandidate text =  DA.decode $ BL.fromStrict $ encodeUtf8 text

encodeCandidate :: Candidate -> Text
encodeCandidate cand = decodeUtf8 $ BL.toStrict $ DA.encode cand
