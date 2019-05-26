{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.DetachHandlerReq where

import Protolude
import qualified Data.Aeson as DA

data DetachHandlerReq = DetachHandlerReq
  { janus :: Text
  , transaction :: Text
  , session_id :: Integer
  , handle_id :: Integer
  } deriving (Show,Generic)

instance DA.ToJSON DetachHandlerReq

detachHandlerReq :: Integer -> Integer -> Integer -> DetachHandlerReq
detachHandlerReq sessID hndID transID =
  DetachHandlerReq
    { janus = "detach"
    , transaction = show transID
    , session_id = sessID
    , handle_id = hndID
    }
