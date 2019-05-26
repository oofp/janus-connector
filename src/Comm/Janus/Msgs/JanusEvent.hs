{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module Comm.Janus.Msgs.JanusEvent where

import Protolude
import qualified Data.Aeson as DA
import qualified Data.ByteString.Lazy as LBStr
import Comm.Janus.Msgs.JSEP
import Text.RawString.QQ
import Prelude (String)

newtype IDData = IDData
  { id :: Integer
  } deriving (Show,Generic)

newtype Result = Result
  { event :: Text
  } deriving (Show,Generic)

data PluginDataContent = PluginDataContent
  {  sip :: Text,
     result :: Result
  } deriving (Show,Generic)

data PluginData = PluginData
  { plugin :: Text
  , pData :: PluginDataContent
  } deriving (Show,Generic)

data JanusEvent = JanusEvent
  { janus :: Text
  , transaction :: Maybe Text
  , session_id :: Maybe Integer
  , sender :: Maybe Integer
  , idData :: Maybe IDData
  , plugindata :: Maybe PluginData
  , jsep :: Maybe JSEP
  } deriving (Show,Generic)

instance DA.FromJSON IDData
instance DA.FromJSON Result
instance DA.FromJSON PluginDataContent

customOptionsPData :: DA.Options
customOptionsPData = DA.defaultOptions
    { DA.fieldLabelModifier = replaceData
    }
  where
    replaceData "pData" = "data"
    replaceData a = a

customOptionsEvent :: DA.Options
customOptionsEvent = DA.defaultOptions
    { DA.fieldLabelModifier = replaceData
    }
  where
    replaceData  "idData" = "data"
    replaceData a = a

instance DA.FromJSON PluginData where
  parseJSON = DA.genericParseJSON customOptionsPData

instance DA.FromJSON JanusEvent where
  parseJSON = DA.genericParseJSON customOptionsEvent

--
sessionCreatedStr::LBStr.ByteString
sessionCreatedStr = [r|{
   "janus": "success",
   "transaction": "3Uxbw3Ym1jxy",
   "data": {
      "id": 6322890423209415
   }
}
|]

ackStr :: LBStr.ByteString
ackStr = [r|{
   "janus": "ack",
   "transaction": "3Uxbw3Ym1jxy"
 }
|]

pluginEventStr :: LBStr.ByteString
pluginEventStr = [r|
{
   "janus": "event",
   "session_id": 6322890423209415,
   "sender": 649781573617088,
   "transaction": "poYo3it4lYtl",
   "plugindata": {
      "plugin": "janus.plugin.sip",
      "data": {
         "sip": "event",
         "result": {
            "event": "registering"
         }
      }
   }
}
|]

pluginEventJSEPStr :: LBStr.ByteString
pluginEventJSEPStr = [r|
{
   "janus": "event",
   "session_id": 6322890423209415,
   "sender": 649781573617088,
   "transaction": "MoMMe4oFUkb5",
   "plugindata": {
      "plugin": "janus.plugin.sip",
      "data": {
         "sip": "event",
         "result": {
            "event": "progress",
            "username": "sip:14163157143@toronto2.voip.ms"
         }
      }
   },
   "jsep": {
      "type": "answer",
      "sdp": "v=0\r\no=root 169224259 169224259 IN IP4 138.68.43.219\r\ns=voip.ms\r\nt=0 0\r\na=group:BUNDLE audio\r\na=msid-semantic: WMS janus\r\nm=audio 9 UDP/TLS/RTP/SAVPF 0 126\r\nc=IN IP4 138.68.43.219\r\na=sendrecv\r\na=mid:audio\r\na=rtcp-mux\r\na=ice-ufrag:pEYd\r\na=ice-pwd:RYDqsudxB3WlYlI32McpTq\r\na=ice-options:trickle\r\na=fingerprint:sha-256 FB:9C:7E:5A:11:D3:F9:52:E6:0F:33:91:71:CF:84:3A:04:C3:70:E9:60:57:28:7A:F1:04:B4:02:5A:C6:3B:9D\r\na=setup:active\r\na=rtpmap:0 PCMU/8000\r\na=rtpmap:126 telephone-event/8000\r\na=fmtp:126 0-16\r\na=ptime:20\r\na=ssrc:4054860913 cname:janusaudio\r\na=ssrc:4054860913 msid:janus janusa0\r\na=ssrc:4054860913 mslabel:janus\r\na=ssrc:4054860913 label:janusa0\r\na=candidate:1 1 udp 2013266431 138.68.43.219 60773 typ host\r\na=candidate:2 1 udp 2013266431 10.46.0.8 38912 typ host\r\na=end-of-candidates\r\n"
   }
}
|]

decodeSessionCreated :: Maybe JanusEvent
decodeSessionCreated=DA.decode sessionCreatedStr

eDecodeSessionCreated :: Either String JanusEvent
eDecodeSessionCreated=DA.eitherDecode sessionCreatedStr

eDecodeAck :: Either String JanusEvent
eDecodeAck=DA.eitherDecode ackStr

eDecodePluginEvent :: Either String JanusEvent
eDecodePluginEvent=DA.eitherDecode pluginEventStr

eDecodePluginEventJSEP :: Either String JanusEvent
eDecodePluginEventJSEP=DA.eitherDecode pluginEventJSEPStr
