module Comm.Janus.JanusConnector
  ( ConHandle
  , JanusServerMsg (..)
  , JanusRegisterEventType (..)
  , JanusCallProgressEventType (..)
  , JanusCallReqPs (..)
  , JanusEchoReqPs (..)
  , JanusVideoCallReqPs (..)
  , JanusVideoCallRegReqPs (..)
  , JanusAudioRoomJoinReqPs (..)
  , JanusAudioRoomConfigureReqPs (..)
  , JanusRegisterReqPs (..)
  , JanusClientMsg (..)
  , ServerHandler
  , ClientHandler
  , PluginType (..)
  , initConnector
  , createHandler
  , sendJanusRequest
  , waitForConnectivity
  , JSEP 
  ) where

import qualified Network.WebSockets  as WS
import Network.WebSockets.Connection
import qualified StmContainers.Map as Map
import Control.Concurrent.STM.TVar
import Protolude
import Prelude (String)
import Data.Text
import Network.Socket (withSocketsDo)
import System.Log.Logger
import qualified Data.Aeson as DA
import qualified Data.ByteString.Lazy as LBStr

import Comm.Janus.Internal.JanusInternalData
import Comm.Janus.Msgs.CreateConReq
import Comm.Janus.Msgs.CreateHandlerReq
import Comm.Janus.Msgs.JanusEvent
import Comm.Janus.Msgs.SessionReq
import Comm.Janus.Msgs.DetachHandlerReq
import qualified Comm.Janus.Msgs.RegisterReq as RR
import qualified Comm.Janus.Msgs.CallReq as CR
import Comm.Janus.Msgs.TrickleReq
import Comm.Janus.Msgs.TrickleConnectedReq
import Comm.Janus.Msgs.HangupReq
import Comm.Janus.Msgs.LeaveReq
import Comm.Janus.Msgs.AcceptReq
import Comm.Janus.Msgs.EchoReq
import qualified Comm.Janus.Msgs.JanusReq as JReq
import qualified Comm.Janus.Msgs.AudioRoomConfigureReq as ARCfgReq
import Comm.Janus.Msgs.AudioRoomJoinReq
import Comm.Janus.Msgs.JSEP
import Comm.Janus.Msgs.Candidate
import Data.HashMap.Strict (lookup)
import qualified Comm.Janus.Msgs.VideoCallCallReq as VCCReq
import qualified Comm.Janus.Msgs.VideoCallRegisterReq as VCRReq

-- |The SIP Registration event type
data JanusRegisterEventType
  = Registering -- ^ Registration initiated and in-progress
  | Registered -- ^ Registration sucessfully completed; You can make call requests now or receive incoming calls
  | RegistrationFailed -- ^ Registration failed (TODO: expose failure code)
  deriving (Show, Eq)

-- |SIP Call progress event type
data JanusCallProgressEventType
  = Calling
  | Progress
  | Proceeding
  | Accepted -- ^ Call was answered by remote party
  deriving (Show, Eq)

-- |Janus Server Msg 
-- |JanusServerMsg is built from events sent by Janus Server
-- |Some events are secific to certain plugin types 
data JanusServerMsg
    = JanusRegisterEvent JanusRegisterEventType -- ^ Registration event (SIP plugin)
    | JanusCallProgressEvent JanusCallProgressEventType (Maybe Text) -- ^ Call progress with optional JSEP (SIP plugin)
    | JanusIncomingCall (Maybe Text) -- ^ Incoming call with option JSEP offer (SIP Plugin) 
    | JanusOKEvent (Maybe Text) -- ^ Generic OK Event with optional JSEP. Used by numnerous plugins
    | JanusWebRtcUp -- ^ Media pass has been successfully establshed
    | JanusHangupEvent -- ^ Call has beem disconnected (SIP plugin); TODO: provide disconnect reason
    deriving (Show, Eq)

-- |Janus Register Request Parameters 
data JanusRegisterReqPs = JanusRegisterReqPs
  { userName :: Text -- ^ User AOR (Address-Of-Record). Like sip:alice@domain.com
  , displayName :: Text -- ^ User display name
  , password :: Text -- ^ SIP Authentocation password
  , proxy :: Text -- ^ Outbound proxy. Like sip:domain.com
  } deriving (Show, Eq)

-- |Janus Call Request Parameters 
data JanusCallReqPs = JanusCallReqPs
  { destUri :: Text -- ^ SIP Address of destination user. Like sip:bob@domain.com
  , sdpOffer :: Text -- ^ JSEP Offer Text (Session Description of local party)
  } deriving (Show, Eq)

-- |Janus Video Call Request Parameters 
data JanusVideoCallReqPs = JanusVideoCallReqPs
  { videoCallDest :: Text -- ^ name of destination user. 
  , videoCallSdpOffer :: Text -- ^ JSEP Offer Text (Session Description of local party)
  } deriving (Show, Eq)

-- |Janus Video Call Registration Parameters 
data JanusVideoCallRegReqPs = JanusVideoCallRegReqPs
  { videoCallUserName :: Text -- ^ name of registering user. 
  } deriving (Show, Eq)

-- |Janus Echo Request Parameters   
data JanusEchoReqPs = JanusEchoReqPs
  { echoAudio :: Bool -- ^ enable audio  
  , echoVideo :: Bool -- ^ enable video
  , echoSdpOffer :: Text -- ^ JSEP Offer Text (Session Description of local party)
  } deriving (Show, Eq)

-- |Janus Audio Room Request Parameters   
data JanusAudioRoomJoinReqPs = JanusAudioRoomJoinReqPs
  { audioRoomNum :: Int -- ^ room number to join (room must preexist)
  , audioRoomDisplay :: Text -- ^ display name of joining party
  } deriving (Show, Eq)


-- |Audio Room Configure Request Parameters   
data JanusAudioRoomConfigureReqPs = JanusAudioRoomConfigureReqPs
  { audioRoomMute :: Bool -- ^ should be muted
  , audioRoomSdpOffer :: Text -- ^ JSEP Offer Text (Session Description of local party)
  } deriving (Show, Eq)

-- |Audio Room Configure Request Parameters   
data JanusClientMsg = JanusDetachHandle -- ^ detach this plugin; ServerHandler is unusable after issuing this request
                    | JanusRegisterReq JanusRegisterReqPs -- ^ Register request (SIP Plugin)
                    | JanusCallReq JanusCallReqPs -- ^ Call request (SIP Plugin)
                    | JanusAcceptReq Text -- ^ Accept incoming call (Accept incoming call)
                    | JanusHangupReq -- ^ Drop SIP Call
                    | JanusLeaveReq -- ^ Leave room or bridge
                    | JanusEchoReq JanusEchoReqPs -- ^ Start Echo Test (EchoTest Plugin)
                    | JanusAudioRoomJoinReq JanusAudioRoomJoinReqPs -- ^ Join audio room (AudioBridge Plugin)
                    | JanusAudioRoomConfigureReq JanusAudioRoomConfigureReqPs -- ^ Configure audio room (AudioBridge Plugin)
                    | JanusVideoCallRegReq JanusVideoCallRegReqPs -- ^ Register request (VideoCall Plugin)
                    | JanusVideoCallReq JanusVideoCallReqPs -- ^ Video Call request (VideoCall Plugin)
                    | JanusTrickleReq Text -- ^ Send ICE Candidate description
                    | JanusIceConnected -- ^ Notify that sending ICE Candidates is completed
                    deriving (Show, Eq)


-- |Represent attached plugin and is used to send JanusClientMsg to Janus server    
type ServerHandler = JanusClientMsg -> IO ()
-- |Passed by Janus client (application) as function - handler of messages sent by Janus   
type ClientHandler = JanusServerMsg -> IO ()

-- |Send request to Janus server
sendJanusRequest :: JanusClientMsg -- ^ Janus client message 
                 -> ServerHandler -- ^ Server Handler
                 -> IO ()
sendJanusRequest =  (&)

data TransData = CreateHandler (MVar (Maybe Integer))

data HandlerState = InitHandlerState ClientHandler

-- |Janus Connector Handle: represents connectivity to one Janus server; 
-- |Once connection is established can be used to connect to different Janus plugins; 
data ConHandle = ConHandle
  { connection :: TVar (Maybe WS.Connection)
  , sessionIDAndKeepaliveTask :: TVar (Maybe (Integer, Async ()))
  , handlers :: Map.Map Integer HandlerState
  , currentTransactions :: Map.Map Integer TransData
  , transactionCounter :: TVar Integer
  , onewayTransactionCounter :: TVar Integer
  }

  
loggerPath::String
loggerPath="JanusCon"

-- |Initialize Janus connector (TODO: Allow both ws and wss, notify when connection got closed) 
initConnector :: Text -- ^ Janus server address (IP or domain)
              -> Int -- ^ Janus server port
              -> IO ConHandle -- ^ ConnectionHandle 
initConnector host port  = do
  conHandle <- atomically $  ConHandle <$> newTVar Nothing <*> newTVar Nothing  <*>  Map.new <*> Map.new <*> newTVar initReqTransID <*> newTVar (-1)
  void $ async $ withSocketsDo $ WS.runClientWith (unpack host) port "/" (WS.defaultConnectionOptions {connectionCompressionOptions=PermessageDeflateCompression defaultPermessageDeflate}) [("Sec-WebSocket-Protocol","janus-protocol")] (app conHandle)
  return conHandle

--------------------------------------------------------------------------------
app :: ConHandle -> WS.ClientApp ()
app connectorHandle conn = do
    debugM loggerPath "connected!"
    atomically $ writeTVar (connection connectorHandle) (Just conn)
    sendRequest (CreateConReq "create" (show createSessionTransID)) conn

    -- Fork a thread that writes WS data to stdout
    void $ forever $ do
        msgRcvd <- WS.receiveData conn
        debugM loggerPath ("Messages received:" <> show msgRcvd)
        handleIncomingMessage connectorHandle msgRcvd

    return ()

newTransID :: ConHandle -> IO Integer
newTransID conHandle =
  let transCounter= transactionCounter conHandle
  in atomically $ do
    curCounter <-readTVar transCounter
    let newCounter = curCounter + 1
    writeTVar transCounter newCounter
    return newCounter

newOnewayTransID :: ConHandle -> IO Integer
newOnewayTransID conHandle =
  let onewayTransCounter = onewayTransactionCounter conHandle
  in atomically $ do
    curCounter <-readTVar onewayTransCounter
    let newCounter = curCounter - 1
    writeTVar onewayTransCounter newCounter
    return newCounter

getSessionID :: ConHandle -> IO (Maybe Integer)
getSessionID conHandle =
  atomically $ do
    sessIDMaybe <- readTVar $ sessionIDAndKeepaliveTask conHandle
    return $ fst <$> sessIDMaybe

-- |Create ServerHandler (connect to Janus plugin).  
createHandler :: ConHandle -- ^ Janus Connection Handle 
              -> PluginType -- ^ Plugin type (SIP, EchoTest, AudioBridge, etc) 
              -> ClientHandler -- ^ Janus message handler
              -> IO (Maybe ServerHandler) -- ^ Janus Server handler
createHandler conHandle janusPlugin clientHandler = do
    sessIDMaybe <- getSessionID conHandle
    case sessIDMaybe of
      Nothing -> return Nothing
      Just sessID -> createHandler' conHandle janusPlugin clientHandler sessID

createHandler' :: ConHandle -> PluginType -> ClientHandler -> Integer -> IO (Maybe ServerHandler)
createHandler' conHandle janusPlugin clientHandler sessID = do
  transID <- newTransID conHandle
  mvar <- newEmptyMVar
  let transData=CreateHandler mvar
  atomically $ Map.insert transData transID (currentTransactions conHandle)
  let createHandlerReqData = createHandlerReq janusPlugin sessID transID
  sendRequest' createHandlerReqData conHandle
  handlerIDMaybe <- readMVar mvar
  forM handlerIDMaybe (\hndID -> do
      atomically $  Map.insert (InitHandlerState clientHandler) hndID (handlers conHandle)
      return $ createServerHandler conHandle sessID hndID)


--------------------------------------------------------------------------------
handleIncomingMessage :: ConHandle -> LBStr.ByteString -> IO  ()
handleIncomingMessage conHandle msgBytes =
  let janusEvEither = DA.eitherDecode msgBytes
  in do
    debugM loggerPath  ("Messages decoded:" <> show janusEvEither)
    case janusEvEither of
      Right janusEv ->
        let transIDMaybe = join $ (readMaybe . unpack) <$> Comm.Janus.Msgs.JanusEvent.transaction janusEv
        in handleJanusEvent janusEv transIDMaybe conHandle
      Left err -> warningM  loggerPath ("Failed to parse janus ev:" <> err <> " ;msg:" <> show msgBytes)

handleJanusEvent :: JanusEvent -> Maybe Integer -> ConHandle -> IO ()
handleJanusEvent janusEv (Just transID) conHandle
    | transID == createSessionTransID = handleSessionCreated janusEv conHandle
    | transID == keepaliveTransID     = handleKeepAliveResp janusEv conHandle
    | transID < 0                     = handleOneWayResponse janusEv conHandle
    | otherwise                       = handleResponse conHandle transID janusEv
handleJanusEvent janusEv Nothing conHandle = do
    sessIDMaybe <- getSessionID conHandle
    forM_ sessIDMaybe (\sessID-> handleSessionResponse conHandle sessID janusEv)

handleSessionCreated :: JanusEvent -> ConHandle -> IO ()
handleSessionCreated janusEv conHandle = case idData janusEv of
  Just idD ->
    let
      sessID = id idD in
    do
      keepaliveTask <- async $ keepAliveProc sessID conHandle
      atomically $ writeTVar (sessionIDAndKeepaliveTask conHandle) (Just (sessID, keepaliveTask))
  Nothing -> warningM  loggerPath ("Failed to retrieve data with session id:" <> show janusEv)

handleOneWayResponse :: JanusEvent -> ConHandle -> IO ()
handleOneWayResponse janusEv _conHandle =
  debugM loggerPath ("one way response received:" <> show janusEv)

handleKeepAliveResp :: JanusEvent -> ConHandle -> IO ()
handleKeepAliveResp janusEv _conHandle =
  debugM loggerPath ("keepalive received:" <> show janusEv)

handleResponse :: ConHandle -> Integer -> JanusEvent -> IO ()
handleResponse conHandle transID janusEv = do
  debugM loggerPath ("handleResponse received:" <> show janusEv)
  reqMaybe <- atomically $ do
    let transMap = currentTransactions conHandle
    r <- Map.lookup transID transMap
    forM_ r (const $ Map.delete transID transMap)
    return r
  case reqMaybe of
    Just (CreateHandler mVar) ->
      let handlerIDMaybe = id <$> idData janusEv
      in putMVar mVar handlerIDMaybe
    _ -> forM_ (Comm.Janus.Msgs.JanusEvent.session_id janusEv) (\sessID->handleSessionResponse conHandle sessID janusEv)

handleSessionResponse :: ConHandle -> Integer -> JanusEvent -> IO ()
handleSessionResponse conHandle _sessID janusEv =
    case (plugindata janusEv, sender janusEv) of
      (Just pluginData, Just senderID) -> case getPluginEvent pluginData of
        Nothing -> return ()
        Just evToSend -> handleEvent senderID evToSend
      (Nothing, Just senderID)-> when (Comm.Janus.Msgs.JanusEvent.janus janusEv == "webrtcup") (handleEvent senderID JanusWebRtcUp)
      _ -> return ()
  where
    getPluginEvent (DA.Object obj) = getPluginDataEvent (lookup "plugin" obj) (lookup "data" obj)  
    getPluginEvent _ = Nothing
    --
    getPluginDataEvent (Just "janus.plugin.sip") (Just (DA.Object sipData)) = 
      getSipResultEvent (lookup "result" sipData)
    getPluginDataEvent (Just "janus.plugin.echotest") (Just (DA.Object sipData)) = 
      getOKResultEvent (lookup "result" sipData)
    getPluginDataEvent (Just "janus.plugin.audiobridge") (Just (DA.Object sipData)) = 
      getOKResultEvent (lookup "result" sipData)
    getPluginDataEvent (Just "janus.plugin.videocall") (Just (DA.Object sipData)) = 
      getVideoCallResultEvent (lookup "result" sipData)
    getPluginDataEvent _ _ = Nothing
    --
    getSipResultEvent (Just (DA.Object resObject))  = lookup "event" resObject >>= getSipEvent 
    getSipResultEvent _ = Nothing
    --
    getOKResultEvent (Just (DA.String "ok"))  = Just $ JanusOKEvent jsp 
    getOKResultEvent _ = Nothing
    --
    getSipEvent "registering" = Just $ JanusRegisterEvent Registering
    getSipEvent "registered" = Just $ JanusRegisterEvent Registered
    getSipEvent "registration_failed" = Just $ JanusRegisterEvent RegistrationFailed
    getSipEvent "incomingcall" = Just $ JanusIncomingCall jsp
    getSipEvent "calling" = Just $ JanusCallProgressEvent Calling jsp
    getSipEvent "progress" = Just $ JanusCallProgressEvent Progress jsp
    getSipEvent "proceeding" = Just $ JanusCallProgressEvent Proceeding jsp
    getSipEvent "accepted" = Just $ JanusCallProgressEvent Accepted jsp
    getSipEvent "hangup" = Just JanusHangupEvent
    getSipEvent _ = Nothing
    --
    getVideoCallResultEvent (Just (DA.Object resObject))  = lookup "event" resObject >>= getVideoCallEvent 
    getVideoCallResultEvent _ = Nothing
    getVideoCallEvent "incomingcall" =  Just $ JanusIncomingCall jsp --TODO: retrieve caller id
    getVideoCallEvent "hangup" = Just JanusHangupEvent
    getVideoCallEvent "accepted" = Just $ JanusCallProgressEvent Accepted jsp
    getVideoCallEvent _ = Nothing
    --
    jsp = encodeJSEP <$> jsep janusEv
    handleEvent senderID evToSend = do
      handlerState <- atomically $ Map.lookup senderID (handlers conHandle)
      forM_ handlerState (\(InitHandlerState clientHnder)-> clientHnder evToSend)

handleClientRequest :: ConHandle -> Integer -> Integer -> JanusClientMsg -> IO ()
handleClientRequest conHandle sessID handlerID JanusDetachHandle = do
  transID <- newOnewayTransID conHandle
  sendRequest' (detachHandlerReq sessID handlerID transID) conHandle
handleClientRequest conHandle sessID handlerID (JanusRegisterReq regReqPs) = do
  transID <- newOnewayTransID conHandle
  sendRequest' (RR.registerReq (userName regReqPs) (displayName regReqPs) (password regReqPs) (proxy regReqPs) sessID handlerID transID) conHandle
handleClientRequest conHandle sessID handlerID (JanusCallReq callReqPs) = do
  infoM  loggerPath ("handleClientRequest (call):" <> show callReqPs)
  let jsepMaybe=decodeJSEP (sdpOffer callReqPs)
  infoM  loggerPath ("handleClientRequest (jsep):" <> show jsepMaybe)
  case jsepMaybe of
    Nothing -> warningM  loggerPath ("Failed to decode sdp offer:" <> show callReqPs)
    Just jsepOffer -> do
      infoM  loggerPath ("handleClientRequest (call) offer decoded:" <> show jsepOffer)
      transID <- newTransID conHandle
      infoM  loggerPath ("handleClientRequest (call) transID:" <> show transID)
      sendRequest' (CR.callReq (destUri callReqPs) jsepOffer sessID handlerID transID) conHandle
handleClientRequest conHandle sessID handlerID (JanusAcceptReq sdpAnswer) = do
  infoM  loggerPath ("handleClientRequest (answer):" <> show sdpAnswer)
  let jsepMaybe=decodeJSEP sdpAnswer
  infoM  loggerPath ("handleClientRequest(accept) (jsep):" <> show jsepMaybe)
  case jsepMaybe of
    Nothing -> warningM  loggerPath ("Failed to decode sdp answer:" <> show sdpAnswer)
    Just jsepAnswer -> do
      infoM  loggerPath ("handleClientRequest (call) answer decoded:" <> show jsepAnswer)
      transID <- newTransID conHandle
      infoM  loggerPath ("handleClientRequest (call) transID:" <> show transID)
      sendRequest' (acceptReq jsepAnswer sessID handlerID transID) conHandle
handleClientRequest conHandle sessID handlerID (JanusTrickleReq candText) =
  case decodeCandidate candText of
    Nothing -> warningM  loggerPath ("Failed to decode candidate:" <> show candText)
    Just cand -> do
      transID <- newOnewayTransID conHandle
      sendRequest' (trickleReq cand sessID handlerID transID) conHandle
handleClientRequest conHandle sessID handlerID JanusIceConnected = do
  transID <- newOnewayTransID conHandle
  infoM  loggerPath ("handleClientRequest (call,JanusIceConnected) transID:" <> show transID)
  sendRequest' (trickleConnectedReq sessID handlerID transID) conHandle
handleClientRequest conHandle sessID handlerID JanusHangupReq = do
  transID <- newOnewayTransID conHandle
  sendRequest' (hangupReq sessID handlerID transID) conHandle
handleClientRequest conHandle sessID handlerID JanusLeaveReq = do
    transID <- newOnewayTransID conHandle
    sendRequest' (leaveReq sessID handlerID transID) conHandle
handleClientRequest conHandle sessID handlerID (JanusEchoReq echoReqPs) = do
  infoM  loggerPath ("handleClientRequest (echo):" <> show echoReqPs)
  let jsepMaybe=decodeJSEP (echoSdpOffer echoReqPs)
  infoM  loggerPath ("handleClientRequest (echo/jsep):" <> show jsepMaybe)
  case jsepMaybe of
    Nothing -> warningM  loggerPath ("Failed to decode (echo/sdp) offer:" <> show echoReqPs)
    Just jsepOffer -> do
      infoM  loggerPath ("handleClientRequest (echo) offer decoded:" <> show jsepOffer)
      transID <- newTransID conHandle
      infoM  loggerPath ("handleClientRequest (echo) transID:" <> show transID)
      sendRequest' (echoReq (echoAudio echoReqPs) (echoVideo echoReqPs) jsepOffer sessID handlerID transID) conHandle
handleClientRequest conHandle sessID handlerID (JanusAudioRoomJoinReq pars) = do
  infoM  loggerPath ("handleClientRequest (JanusAudioRoomJoinReq):" <> show pars)
  transID <- newTransID conHandle
  sendRequest' (audioRoomJoinReq (audioRoomNum pars) (audioRoomDisplay pars) sessID handlerID transID) conHandle
handleClientRequest conHandle sessID handlerID (JanusAudioRoomConfigureReq pars) = do
  infoM  loggerPath ("handleClientRequest (JanusAudioRoomConfigureReq):" <> show pars)
  let jsepMaybe=decodeJSEP (audioRoomSdpOffer pars)
  infoM  loggerPath ("handleClientRequest (audioRoom/jsep):" <> show jsepMaybe)
  case jsepMaybe of
    Nothing -> warningM  loggerPath ("Failed to decode (audioRoom/sdp) offer:" <> show pars)
    Just jsepOffer -> do
      infoM  loggerPath ("handleClientRequest (audioRoom) offer decoded:" <> show jsepOffer)
      transID <- newTransID conHandle
      infoM  loggerPath ("handleClientRequest (audioRoom) transID:" <> show transID)
      sendRequest' (ARCfgReq.audioRoomConfigureReq (audioRoomMute pars) jsepOffer sessID handlerID transID) conHandle
handleClientRequest conHandle sessID handlerID (JanusVideoCallReq pars) = do
  infoM  loggerPath ("handleClientRequest (JanusVideoCallReq):" <> show pars)
  let jsepMaybe=decodeJSEP (videoCallSdpOffer pars)
  infoM  loggerPath ("handleClientRequest (videoCall/jsep):" <> show jsepMaybe)
  case jsepMaybe of
    Nothing -> warningM  loggerPath ("Failed to decode (videoRoom/sdp) offer:" <> show pars)
    Just jsepOffer -> do
      infoM  loggerPath ("handleClientRequest (videoCall) offer decoded:" <> show jsepOffer)
      transID <- newTransID conHandle
      infoM  loggerPath ("handleClientRequest (videoRoom) transID:" <> show transID)
      sendRequest' (JReq.buildJanusReqWithJsep (VCCReq.VideoCallCallReqPs (videoCallDest pars)) jsepOffer sessID handlerID transID) conHandle
handleClientRequest conHandle sessID handlerID (JanusVideoCallRegReq pars) = do
  infoM  loggerPath ("handleClientRequest (JanusVideoCallRegReq):" <> show pars)
  transID <- newTransID conHandle
  sendRequest' (JReq.buildJanusReq (VCRReq.VideoCallRegisterReqPs (videoCallUserName pars)) sessID handlerID transID) conHandle
                
createServerHandler :: ConHandle -> Integer -> Integer -> ServerHandler
createServerHandler = handleClientRequest

keepAliveProc :: Integer -> ConHandle -> IO ()
keepAliveProc sessID conHandle=
    let keepAliveMsg = SessionReq "keepalive" (show keepaliveTransID) sessID
    in go keepAliveMsg
  where
    go kaMsg = do
      threadDelay 25000000
      sendRequest' kaMsg conHandle
      debugM loggerPath "keepalive sent"
      go kaMsg

sendRequest :: DA.ToJSON obj => obj -> WS.Connection -> IO ()
sendRequest obj conn =  WS.sendTextData conn (DA.encode obj)

sendRequest' :: DA.ToJSON obj => obj -> ConHandle -> IO ()
sendRequest' obj conHandle = do
  debugM  loggerPath ("sendRequest':" <> show (DA.encode obj))
  connMaybe <- readTVarIO (connection conHandle)
  forM_ connMaybe (sendRequest obj)

isJanusConnected :: ConHandle -> STM Bool
isJanusConnected  conHandle = fmap isJust (readTVar $ sessionIDAndKeepaliveTask conHandle)

-- |Block until connectivity with Janus is established (TODO: Consider changing to withConnector)  
waitForConnectivity :: ConHandle -- ^ Janus Connection Handle 
                    -> IO () 
waitForConnectivity conHandle = atomically $ do 
  isConnected <- isJanusConnected conHandle
  unless isConnected retry 


{- no needed  
modifyJSEPTxt ::  Text -> (Text -> Text) -> Maybe Text
modifyJSEPTxt jsepTxt f = 
 case decodeJSEP jsepTxt of
  Nothing ->Nothing
  Just curJsep -> Just $ encodeJSEP (JSEP (offerAnswer curJsep) (f (sdp curJsep)) (trickle curJsep))


setJSEPTrickle ::  Text -> Maybe Bool -> Maybe Text
setJSEPTrickle jsepTxt tr = 
  case decodeJSEP jsepTxt of
    Nothing ->Nothing
    Just curJsep -> Just $ encodeJSEP $ curJsep {trickle = tr}
  
-}
