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

main :: IO ()
main = do
  putStrLn ("Welcome to Janus sample app "::Text)
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


data PartyType 
  = VideoParty            
  | SipParty
  deriving (Show, Eq)

data PartyMessage = PartyMessage 
  { partyType :: PartyType
  , msg :: JanusServerMsg 
  }

runTest :: Text -> Int -> JanusRegisterReqPs -> Text -> Text -> IO ()
runTest srvIP port janusRegisterReqPs videoDest sipDest = do
  setupLog
  connectorHandle <- initConnector srvIP port
  waitForConnectivity connectorHandle
  msgChan <- newTChanIO   
  srvHandlerMaybe <- createHandler connectorHandle Sip (janusCallback msgChan SipParty)
  srvHandlerMaybe2 <- createHandler connectorHandle VideoCall (janusCallback msgChan VideoParty) 
  case (srvHandlerMaybe, srvHandlerMaybe2) of
    (Just sipHandler, Just videoCallHandler) -> do
      sendJanusRequest (JanusRegisterReq janusRegisterReqPs) sipHandler  
      sendJanusRequest (JanusVideoCallRegReq (JanusVideoCallRegReqPs "Bob")) videoCallHandler  
      channelLoop msgChan sipHandler videoCallHandler videoDest sipDest 
      putStrLn ("Press [Enter] to exit"::Text)
      void getLine
    _ -> putStrLn ("Failed to create some handlers"::Text)
   
janusCallback :: TChan PartyMessage -> PartyType -> JanusServerMsg  -> IO ()
janusCallback chan thisParty janusServerMsg = do
  putStrLn $ ("Janus event received (SIP)/"::Text) <> show thisParty <> ("-->"::Text) <> show janusServerMsg
  atomically $ writeTChan chan (PartyMessage thisParty janusServerMsg)
  
channelLoop :: TChan PartyMessage -> ServerHandler -> ServerHandler -> Text -> Text -> IO ()
channelLoop msgChan sipHandler videoCallHandler videoDest sipDest = 
    void $ goLoop False 
  where
    goLoop :: Bool -> IO Bool
    goLoop flUseSipDecline = do
      (PartyMessage msgParty newMsg) <- atomically $ readTChan msgChan
      flNewAnswered <- case (msgParty, newMsg) of 
        (SipParty, (JanusIncomingCall (Just offer))) -> 
          sendJanusRequest (JanusVideoCallReq (JanusVideoCallReqPs videoDest offer)) videoCallHandler >> return True
        (VideoParty,JanusIncomingCall (Just offer)) -> do 
          putStrLn ("************ Send MakeCall request to SIP"::Text)
          sendJanusRequest (JanusCallReq (JanusCallReqPs sipDest offer)) sipHandler >> return False
        (party, JanusCallProgressEvent  Accepted (Just answer)) ->
          sendJanusRequest (JanusAcceptReq answer) (otherHandler party) >> return False
        (party, JanusWebRtcUp) -> putStrLn (("************ Media path established#"::Text) <> show party) >> return flUseSipDecline
        (party, JanusHangupEvent) -> do 
          if flUseSipDecline && party == VideoParty
            then sendJanusRequest JanusDeclineReq (otherHandler party)  
            else sendJanusRequest JanusHangupReq (otherHandler party)
          return flUseSipDecline  
        _ -> return flUseSipDecline -- do nothing
      goLoop flNewAnswered
    otherHandler SipParty = videoCallHandler      
    otherHandler VideoParty = sipHandler      

setupLog::IO()
setupLog=do
  let rootLog = "JanusCon"

  updateGlobalLogger rootLog (setLevel DEBUG)
  h <- fileHandler "janusSampleLog.log" DEBUG >>= \lh -> return $
              setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")

  updateGlobalLogger rootLog (addHandler h)

  debugM rootLog "Logger started"
  


-- C:\repo\trunk\androidApps\hs\wr\janus\.stack-work\install\4ce01a7c\doc\all\index.html  