{-# LANGUAGE DeriveGeneric #-}

module Comm.Janus.Msgs.LeaveReq where

import Protolude
import qualified Data.Aeson as DA

{-
{"janus":"message","body":{"request":"hangup"},"transaction":"TqxCuGdiHDxS","session_id":8789546932689362,"handle_id":1282395627757395}
-}

newtype LeaveReqBody = LeaveReqBody
  { request :: Text
  } deriving (Show,Generic)

data LeaveReq = LeaveReq
  { janus :: Text
  , body :: LeaveReqBody
  , transaction :: Text
  , session_id :: Integer
  , handle_id :: Integer
  } deriving (Show,Generic)

instance DA.ToJSON LeaveReqBody
instance DA.ToJSON LeaveReq

leaveReq :: Integer -> Integer -> Integer -> LeaveReq
leaveReq sessID hndID transID =
  let leaveBody = LeaveReqBody
                  { request = "leave"
                  }
  in LeaveReq
    { janus = "message"
    , transaction = show transID
    , session_id = sessID
    , handle_id = hndID
    , body = leaveBody
    }
