{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.CallReq where

import Protolude
import qualified Data.Aeson as DA
import Comm.Janus.Msgs.JSEP

{-
{
  "janus": "message",
  "body": {
    "request": "call",
    "uri": "sip:14163157143@toronto2.voip.ms"
  },
  "transaction": "MoMMe4oFUkb5",
  "jsep": {
    "type": "offer",
    "sdp": "v=0\r\no=- 4177329690505199718 2 IN IP4 127.0.0.1\r\ns=-\r\nt=0 0\r\na=group:BUNDLE audio\r\na=msid-semantic: WMS 95SVI5TPGJyt5dj2lOhhQ37LizD23ID2Vfl5\r\nm=audio 9 UDP/TLS/RTP/SAVPF 111 103 104 9 0 8 106 105 13 110 112 113 126\r\nc=IN IP4 0.0.0.0\r\na=rtcp:9 IN IP4 0.0.0.0\r\na=ice-ufrag:Shll\r\na=ice-pwd:XvMUGR1qbBjG6bC3blelzr+6\r\na=ice-options:trickle\r\na=fingerprint:sha-256 9A:4F:13:E0:36:AA:2D:17:7F:94:E8:29:06:1E:87:D7:CF:2F:06:05:BD:AE:6A:FE:20:47:81:98:EA:FF:8E:92\r\na=setup:actpass\r\na=mid:audio\r\na=extmap:1 urn:ietf:params:rtp-hdrext:ssrc-audio-level\r\na=sendrecv\r\na=rtcp-mux\r\na=rtpmap:111 opus/48000/2\r\na=rtcp-fb:111 transport-cc\r\na=fmtp:111 minptime=10;useinbandfec=1\r\na=rtpmap:103 ISAC/16000\r\na=rtpmap:104 ISAC/32000\r\na=rtpmap:9 G722/8000\r\na=rtpmap:0 PCMU/8000\r\na=rtpmap:8 PCMA/8000\r\na=rtpmap:106 CN/32000\r\na=rtpmap:105 CN/16000\r\na=rtpmap:13 CN/8000\r\na=rtpmap:110 telephone-event/48000\r\na=rtpmap:112 telephone-event/32000\r\na=rtpmap:113 telephone-event/16000\r\na=rtpmap:126 telephone-event/8000\r\na=ssrc:4125523353 cname:HgchVQaqnPGD7++N\r\na=ssrc:4125523353 msid:95SVI5TPGJyt5dj2lOhhQ37LizD23ID2Vfl5 70dde2f8-3a8c-4f23-8f45-d799d640943f\r\na=ssrc:4125523353 mslabel:95SVI5TPGJyt5dj2lOhhQ37LizD23ID2Vfl5\r\na=ssrc:4125523353 label:70dde2f8-3a8c-4f23-8f45-d799d640943f\r\n"
  },
  "session_id": 6322890423209415,
  "handle_id": 649781573617088
}
-}

--TODO: allow passing secret in CallReq with no need to register first when making outbound call
data CallReqBody = CallReqBody
  { request :: Text -- call
  , uri :: Text
  } deriving (Show,Generic)

data CallReq = CallReq
  { janus :: Text
  , body :: CallReqBody
  , jsep :: JSEP
  , transaction :: Text
  , session_id :: Integer
  , handle_id :: Integer
  } deriving (Show,Generic)

instance DA.ToJSON CallReqBody
instance DA.ToJSON CallReq

callReq :: Text -> JSEP -> Integer -> Integer -> Integer -> CallReq
callReq destUri offerAnswerSDP sessID hndID transID =
  let callBody = CallReqBody
                  { request = "call"
                  , uri = destUri
                  }
  in CallReq
    { janus = "message"
    , transaction = show transID
    , session_id = sessID
    , handle_id = hndID
    , body = callBody
    , jsep = offerAnswerSDP
    }
