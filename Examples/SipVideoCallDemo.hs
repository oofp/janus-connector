module Main where

import Protolude
import System.Environment (getArgs)
import Comm.Janus.JanusConnector
import Data.Text (pack, replace)
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
    [srvIP, portTxt, sipAOR, sipDisplayName, sipPwd, sipProxy, dest] ->
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
          in runTest (pack srvIP) srvPort regReq (pack dest)
        Nothing ->
          putStrLn ("Failed to parse sip port (must be Int)"::Text)
    _ -> do putStrLn ("Parametrs: srvIP srvPort sipAOR sipDisplayName sipPwd sipProxy dest"::Text)        
            putStrLn ("For example: JanusSample janusserver.com 8188 sip:user@sipprovider.com John 123456 sip:sipprovider.com Bob"::Text)        


runTest :: Text -> Int -> JanusRegisterReqPs -> Text -> IO ()
runTest srvIP port janusRegisterReqPs dest = do
  setupLog
  connectorHandle <- initConnector srvIP port
  waitForConnectivity connectorHandle
  msgChan <- newTChanIO   
  srvHandlerMaybe <- createHandler connectorHandle Sip (janusCallback msgChan 0)
  srvHandlerMaybe2 <- createHandler connectorHandle VideoCall (janusCallback msgChan 1) 
  case (srvHandlerMaybe, srvHandlerMaybe2) of
    (Just sipHandler, Just videoCallHandler) -> do
      sendJanusRequest (JanusRegisterReq janusRegisterReqPs) sipHandler  
      sendJanusRequest (JanusVideoCallRegReq (JanusVideoCallRegReqPs "sipAgent")) videoCallHandler  
      channelLoop msgChan sipHandler videoCallHandler dest
      putStrLn ("Press [Enter] to exit"::Text)
      void getLine
    _ -> putStrLn ("Failed to create some handlers"::Text)
   
janusCallback :: TChan (Int,JanusServerMsg) -> Int -> JanusServerMsg -> IO ()
janusCallback chan num janusServerMsg = do
  putStrLn $ ("Janus event received (SIP)/"::Text) <> show num <> ("-->"::Text) <> show janusServerMsg
  atomically $ writeTChan chan (num,janusServerMsg)
  
adjustTrickle :: Text -> Text
adjustTrickle sdpTxt = Data.Text.replace "a=ice-options:trickle" "a=trickle:false" sdpTxt

channelLoop :: TChan (Int,JanusServerMsg) -> ServerHandler -> ServerHandler -> Text -> IO ()
channelLoop msgChan sipHandler videoCallHandler dest = 
    goLoop
  where
    goLoop = do
      (num, msg) <- atomically $ readTChan msgChan
      case (num,msg) of 
        (0, JanusIncomingCall (Just offer)) -> do
          sendJanusRequest (JanusVideoCallReq (JanusVideoCallReqPs dest offer)) videoCallHandler 
        (1, JanusCallProgressEvent  Accepted (Just answer)) -> do
          sendJanusRequest (JanusAcceptReq answer) sipHandler
        (partyNum,JanusWebRtcUp) -> putStrLn (("************ Media path established#"::Text) <> show partyNum)
        (1,JanusIncomingCall (Just _sdpOffer)) -> sendJanusRequest JanusHangupReq videoCallHandler -- TODO: support call in opposite direction
        (0,JanusHangupEvent) -> sendJanusRequest JanusHangupReq videoCallHandler  
        (1,JanusHangupEvent) -> sendJanusRequest JanusHangupReq sipHandler  
        _ -> return ()
      goLoop    

setupLog::IO()
setupLog=do
  let rootLog = "JanusCon"

  updateGlobalLogger rootLog (setLevel DEBUG)
  h <- fileHandler "janusSampleLog.log" DEBUG >>= \lh -> return $
              setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")

  updateGlobalLogger rootLog (addHandler h)

  debugM rootLog "Logger started"
  


-- C:\repo\trunk\androidApps\hs\wr\janus\.stack-work\install\4ce01a7c\doc\all\index.html  