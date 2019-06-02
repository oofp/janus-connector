{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Comm.Janus.Msgs.JanusReq where

import Protolude
import qualified Data.Aeson as DA
import Comm.Janus.Msgs.JSEP

data JanusReq bodyType = JanusReq 
  { janus :: Text
  , body :: bodyType
  , transaction :: Text
  , session_id :: Integer
  , handle_id :: Integer
  , jsep :: Maybe JSEP
  } deriving (Show,Generic)

data JSepK 
  = JSepTrue   
  | JSepFalse
  | JsepMaybe

class JanusBodyBuilder reqPs bodyType (jsep :: JSepK) | reqPs -> bodyType jsep where
  createReqBody :: reqPs -> bodyType
  

omitNothingOpt :: DA.Options
omitNothingOpt = DA.defaultOptions
    { DA.omitNothingFields = True
    }

instance DA.ToJSON bodyType =>  DA.ToJSON  (JanusReq bodyType) where
  toJSON     = DA.genericToJSON omitNothingOpt
  toEncoding = DA.genericToEncoding omitNothingOpt
  
buildJanusReq :: (JanusBodyBuilder reqPs bodyType 'JSepFalse, DA.ToJSON bodyType) => reqPs -> Integer -> Integer -> Integer -> JanusReq bodyType
buildJanusReq reqPs sessID hndID transID =
  let reqBody = createReqBody reqPs
  in JanusReq
    { janus = "message"
    , transaction = show transID
    , session_id = sessID
    , handle_id = hndID
    , body = reqBody
    , jsep = Nothing
    }

buildJanusReqWithJsep :: (JanusBodyBuilder reqPs bodyType 'JSepTrue, DA.ToJSON bodyType) => reqPs -> JSEP -> Integer -> Integer -> Integer -> JanusReq bodyType
buildJanusReqWithJsep reqPs jsepPar sessID hndID transID =
  let reqBody = createReqBody reqPs  
  in JanusReq
    { janus = "message"
    , transaction = show transID
    , session_id = sessID
    , handle_id = hndID
    , body = reqBody
    , jsep = Just jsepPar
    }
