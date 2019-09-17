{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.DeclineReq where

import Protolude
import qualified Data.Aeson as DA

{-
{"janus":"message","body":{"request":"hangup"},"transaction":"TqxCuGdiHDxS","session_id":8789546932689362,"handle_id":1282395627757395}
-}

newtype DeclineReqBody = DeclineReqBody
  { request :: Text
  } deriving (Show,Generic)

data HangupReq = HangupReq
  { janus :: Text
  , body :: DeclineReqBody
  , transaction :: Text
  , session_id :: Integer
  , handle_id :: Integer
  } deriving (Show,Generic)

instance DA.ToJSON DeclineReqBody
instance DA.ToJSON HangupReq

declineReq :: Integer -> Integer -> Integer -> HangupReq
declineReq sessID hndID transID =
  let declineBody = DeclineReqBody
                  { request = "decline"
                  }
  in HangupReq
    { janus = "message"
    , transaction = show transID
    , session_id = sessID
    , handle_id = hndID
    , body = declineBody
    }
