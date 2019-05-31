{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.CreateHandlerReq where

import Protolude
import qualified Data.Aeson as DA
import Data.Text (toLower)

data CreateHandlerReq = CreateHandlerReq
  { janus :: Text
  , plugin :: Text
  , opaque_id :: Text
  , transaction :: Text
  , force_bundle :: Bool
  , force_rtcp_mux :: Bool
  , session_id :: Integer
  } deriving (Show,Generic)

customOptions :: DA.Options
customOptions = DA.defaultOptions
    { DA.fieldLabelModifier = replaceData
    }
  where
    replaceData "force_bundle" = "force-bundle"
    replaceData "force-rtcp-mux" = "force-rtcp-mux"
    replaceData a = a

instance DA.ToJSON CreateHandlerReq where
  toJSON     = DA.genericToJSON customOptions
  toEncoding = DA.genericToEncoding customOptions

createHandlerReq :: PluginType -> Integer -> Integer -> CreateHandlerReq
createHandlerReq pluginType sessID transID =
  CreateHandlerReq
    { janus = "attach"
    , plugin = pluginName pluginType
    , opaque_id = show sessID <> "." <> show transID
    , transaction = show transID
    , force_bundle = True
    , force_rtcp_mux = True
    , session_id = sessID
    }

data PluginType 
  = Sip
  | EchoTest
  | VoiceMail
  | AudioBridge
  deriving Show
  
pluginName :: PluginType -> Text
pluginName = ((<>) "janus.plugin.") . toLower . show  
            
{-    
createSIPHandlerReq :: Integer -> Integer -> CreateHandlerReq
createSIPHandlerReq = createHandlerReq "janus.plugin.sip"

createEchoHandlerReq :: Integer -> Integer -> CreateHandlerReq
createEchoHandlerReq = createHandlerReq "janus.plugin.echotest"
-}


-- "janus":"attach","plugin":"janus.plugin.sip","opaque_id":"siptest-mXBwVOTlDmM7","transaction":"71eiU1CwLX4Y","force-bundle":true,"force-rtcp-mux":true,"session_id":6322890423209415
