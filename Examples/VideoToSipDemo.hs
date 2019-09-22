module Main where

import Protolude
import System.Environment (getArgs)
import Comm.Janus.JanusConnector
import Data.Text (pack)
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.Log.Handler.Simple (fileHandler)
import Control.Concurrent.STM.TChan

type ServerAddress = Text
type Port = Int
type VideoUserName = Text
type SipAddress = Text 

main :: IO ()
main = do
  putStrLn ("Welcome to Video To Sip Janus demo app "::Text)
  args <- getArgs
  case args of 
    [srvIP, portTxt, sipAOR, sipDisplayName, sipPwd, sipProxy, videoDest, sipDest] ->
      let srvPortMaybe :: Maybe Int
          srvPortMaybe = readMaybe portTxt
      in case srvPortMaybe of
        Just srvPort -> 
          let regReq = JanusRegisterReqPs
                          { userName = pack sipAOR
                          , displayName = pack sipDisplayName
                          , password = pack sipPwd
                          , proxy = pack sipProxy
                          }
          in runTest (pack srvIP) srvPort regReq (pack videoDest) (pack sipDest)
        Nothing ->
          putStrLn ("Failed to parse sip port (must be Int)"::Text)
    _ -> do putStrLn ("Parametrs: srvIP srvPort sipAOR sipDisplayName sipPwd sipProxy videoDest sipDest"::Text)        
            putStrLn ("For example: JanusSample janusserver.com 8188 sip:user@sipprovider.com John 123456 sip:sipprovider.com Bob Alice@sipprovider.com"::Text)        

-- Example of Haskell enum (sum type)
data PartyType 
  = VideoParty            
  | SipParty
  deriving (Show, Eq)

-- Example of Haskell record (sum type)
data PartyMessage = PartyMessage 
  { partyType :: PartyType
  , msg :: JanusServerMsg 
  }

-- VideoCall To Sip plugin call  
runTest :: ServerAddress -> Port -> JanusRegisterReqPs -> VideoUserName -> SipAddress -> IO ()  
runTest srvIP port janusRegisterReqPs videoUserName sipAddress = do
  setupLog
  -- connecting to Janus
  connectorHandle <- initConnector srvIP port --notice type inference we don't specify types , it is still typesafe 
  waitForConnectivity connectorHandle
  -- create queue where both incoming video and SIP plugins messages are placed
  msgQueue <- newTChanIO                              
  -- attach to SIP plugin, pass function that places incoming Sip message to the queue (partially applied)
  sipPluginMaybe <- createHandler connectorHandle Sip (janusCallback msgQueue SipParty)
  -- attach to Video plugin, pass function that places incoming Vido message to the queue (partially applied)
  videoPluginMaybe <- createHandler connectorHandle VideoCall (janusCallback msgQueue VideoParty) 
  -- proceed with successfully attached to both plugins
  case (sipPluginMaybe, videoPluginMaybe) of
    (Just sipPlugin, Just videoPlugin) -> do
      -- SIP registration request 
      sendJanusRequest (JanusRegisterReq janusRegisterReqPs) sipPlugin  
      -- Video user registration
      sendJanusRequest (JanusVideoCallRegReq (JanusVideoCallRegReqPs videoUserName)) videoPlugin  
      -- run processing loop in separate thread
      asyncTask <- async (channelLoop msgQueue sipPlugin videoPlugin sipAddress) 
      putStrLn ("Press [Enter] to exit"::Text)
      void getLine
      -- cancel processing loop
      cancel asyncTask
    _ -> putStrLn ("Failed to create some handlers"::Text)
   
janusCallback :: TChan PartyMessage -> PartyType -> JanusServerMsg  -> IO ()
janusCallback chan thisParty janusServerMsg = do
  putStrLn $ ("Janus message received"::Text) <> show thisParty <> ("-->"::Text) <> show janusServerMsg
  atomically $ writeTChan chan (PartyMessage thisParty janusServerMsg)
  
channelLoop :: TChan PartyMessage -- queue of incoming messages 
            -> ServerHandler -> ServerHandler -> SipAddress -> IO ()
channelLoop msgQueue sipPlugin videoPlugin sipDest = 
    forever $ do
      (PartyMessage msgParty newMsg) <- atomically $ readTChan msgQueue
      case (msgParty, newMsg) of 
        (VideoParty,JanusIncomingCall (Just offer)) ->  
          sendJanusRequest (JanusCallReq (JanusCallReqPs sipDest offer)) sipPlugin 
        (SipParty, JanusCallProgressEvent  Accepted (Just answer)) ->
          sendJanusRequest (JanusAcceptReq answer) videoPlugin 
        (party, JanusWebRtcUp) -> putStrLn (("JanusWebRtcUp"::Text) <> show party)
        (party, JanusHangupEvent) -> sendJanusRequest JanusHangupReq (otherPlugin party)
        _ -> return ()
  where
    otherPlugin SipParty = videoPlugin      
    otherPlugin VideoParty = sipPlugin      

setupLog::IO()
setupLog=do
  let rootLog = "JanusCon"

  updateGlobalLogger rootLog (setLevel DEBUG)
  h <- fileHandler "janusSampleLog.log" DEBUG >>= \lh -> return $
              setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")

  updateGlobalLogger rootLog (addHandler h)

  debugM rootLog "Logger started"
  


-- C:\repo\trunk\androidApps\hs\wr\janus\.stack-work\install\4ce01a7c\doc\all\index.html  