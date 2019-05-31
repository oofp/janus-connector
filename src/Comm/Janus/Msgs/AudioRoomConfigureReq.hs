{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.AudioRoomConfigureReq where

import Protolude
import qualified Data.Aeson as DA
import Comm.Janus.Msgs.JSEP

{-
 {"janus":"message","body":{"request":"configure","muted":false},"transaction":"GDV58dPDICHz","jsep":{"type":"offer","sdp":"v=0\r\no=- 4968381484329215690 2 IN IP4 127.0.0.1\r\ns=-\r\nt=0 0\r\na=group:BUNDLE 0\r\na=msid-semantic: WMS 0ZsyeJw9mG1thYFQnroznzHc9w3jU6AdNvzG\r\nm=audio 9 UDP/TLS/RTP/SAVPF 111 103 104 9 0 8 106 105 13 110 112 113 126\r\nc=IN IP4 0.0.0.0\r\na=rtcp:9 IN IP4 0.0.0.0\r\na=ice-ufrag:1r/2\r\na=ice-pwd:s0WVlMIIH24eUKreMsik8DEv\r\na=ice-options:trickle\r\na=fingerprint:sha-256 34:2A:A9:31:19:D9:A9:1C:C1:74:77:04:A1:47:1B:C9:28:2B:76:A0:38:53:FB:E3:48:1F:C7:2B:2C:29:24:CD\r\na=setup:actpass\r\na=mid:0\r\na=extmap:1 urn:ietf:params:rtp-hdrext:ssrc-audio-level\r\na=extmap:2 http://www.ietf.org/id/draft-holmer-rmcat-transport-wide-cc-extensions-01\r\na=extmap:3 urn:ietf:params:rtp-hdrext:sdes:mid\r\na=extmap:4 urn:ietf:params:rtp-hdrext:sdes:rtp-stream-id\r\na=extmap:5 urn:ietf:params:rtp-hdrext:sdes:repaired-rtp-stream-id\r\na=sendrecv\r\na=msid:0ZsyeJw9mG1thYFQnroznzHc9w3jU6AdNvzG dc3d9cb4-b5ad-4673-8db9-9e91f0406017\r\na=rtcp-mux\r\na=rtpmap:111 opus/48000/2\r\na=rtcp-fb:111 transport-cc\r\na=fmtp:111 minptime=10;useinbandfec=1\r\na=rtpmap:103 ISAC/16000\r\na=rtpmap:104 ISAC/32000\r\na=rtpmap:9 G722/8000\r\na=rtpmap:0 PCMU/8000\r\na=rtpmap:8 PCMA/8000\r\na=rtpmap:106 CN/32000\r\na=rtpmap:105 CN/16000\r\na=rtpmap:13 CN/8000\r\na=rtpmap:110 telephone-event/48000\r\na=rtpmap:112 telephone-event/32000\r\na=rtpmap:113 telephone-event/16000\r\na=rtpmap:126 telephone-event/8000\r\na=ssrc:2919594309 cname:dyPIjKne2FO+w3uW\r\na=ssrc:2919594309 msid:0ZsyeJw9mG1thYFQnroznzHc9w3jU6AdNvzG dc3d9cb4-b5ad-4673-8db9-9e91f0406017\r\na=ssrc:2919594309 mslabel:0ZsyeJw9mG1thYFQnroznzHc9w3jU6AdNvzG\r\na=ssrc:2919594309 label:dc3d9cb4-b5ad-4673-8db9-9e91f0406017\r\n"}}
 -}

data AudioRoomConfigureReqBody = AudioRoomConfigureReqBody
  { request :: Text -- configure
  , mute :: Bool
  } deriving (Show,Generic)

data AudioRoomConfigureReq = AudioRoomConfigureReq
  { janus :: Text
  , body :: AudioRoomConfigureReqBody
  , jsep :: JSEP
  , transaction :: Text
  , session_id :: Integer
  , handle_id :: Integer
  } deriving (Show,Generic)

instance DA.ToJSON AudioRoomConfigureReqBody
instance DA.ToJSON AudioRoomConfigureReq

audioRoomConfigureReq :: Bool -> JSEP -> Integer -> Integer -> Integer -> AudioRoomConfigureReq
audioRoomConfigureReq flMute offerSDP sessID hndID transID =
  let reqBody = AudioRoomConfigureReqBody
                  { request = "configure"
                  , mute = flMute
                  }
  in AudioRoomConfigureReq
    { janus = "message"
    , transaction = show transID
    , session_id = sessID
    , handle_id = hndID
    , body = reqBody
    , jsep = offerSDP
    }
