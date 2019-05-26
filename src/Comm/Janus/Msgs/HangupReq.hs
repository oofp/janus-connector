{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.HangupReq where

import Protolude
import qualified Data.Aeson as DA

{-
{"janus":"message","body":{"request":"hangup"},"transaction":"TqxCuGdiHDxS","session_id":8789546932689362,"handle_id":1282395627757395}
-}

newtype HangupReqBody = HangupReqBody
  { request :: Text
  } deriving (Show,Generic)

data HangupReq = HangupReq
  { janus :: Text
  , body :: HangupReqBody
  , transaction :: Text
  , session_id :: Integer
  , handle_id :: Integer
  } deriving (Show,Generic)

instance DA.ToJSON HangupReqBody
instance DA.ToJSON HangupReq

hangupReq :: Integer -> Integer -> Integer -> HangupReq
hangupReq sessID hndID transID =
  let hangupBody = HangupReqBody
                  { request = "hangup"
                  }
  in HangupReq
    { janus = "message"
    , transaction = show transID
    , session_id = sessID
    , handle_id = hndID
    , body = hangupBody
    }
