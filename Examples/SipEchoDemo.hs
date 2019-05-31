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
--import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  putStrLn ("Welcome to Janus sip echo demo app "::Text)
  args <- getArgs
  case args of 
    [srvIP, portTxt, sipAOR, sipDisplayName, sipPwd, sipProxy] ->
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
          in runTest (pack srvIP) srvPort regReq 
        Nothing ->
          putStrLn ("Failed to parse sip port (must be Int)"::Text)
    _ -> do putStrLn ("Parametrs: srvIP srvPort sipAOR sipDisplayName sipPwd sipProxy"::Text)        
            putStrLn ("For example: SipEchoDemo janusserver.com 8188 sip:user@sipprovider.com John 123456 sip:18001234567@sipprovider.com"::Text)        


runTest :: Text -> Int -> JanusRegisterReqPs -> IO ()
runTest srvIP port janusRegisterReqPs = do
  setupLog
  connectorHandle <- initConnector srvIP port
  connectorHandle2 <- initConnector srvIP port
  waitForConnectivity connectorHandle
  waitForConnectivity connectorHandle2
  msgChan <- newTChanIO   
  srvHandlerMaybe <- createHandler connectorHandle Sip (janusSipCallback msgChan)
  echoHandlerMaybe <- createHandler connectorHandle2 EchoTest (janusEchoCallback msgChan) 
  case (srvHandlerMaybe, echoHandlerMaybe) of
    (Just sipHandler, Just echoHandler) -> do
      sendJanusRequest (JanusRegisterReq janusRegisterReqPs) sipHandler  
      channelLoop msgChan sipHandler echoHandler
      putStrLn ("Press [Enter] to exit"::Text)
      void getLine
    _ -> putStrLn ("Failed to create some handlers"::Text)

data MsgSource 
  = SipMsg
  | EchoMsg

janusSipCallback :: TChan (MsgSource,JanusServerMsg) -> JanusServerMsg -> IO ()
janusSipCallback chan janusServerMsg = do
  putStrLn $ ("Janus event received (SIP)/"::Text) <> ("-->"::Text) <> show janusServerMsg
  atomically $ writeTChan chan (SipMsg,janusServerMsg)
  
janusEchoCallback :: TChan (MsgSource,JanusServerMsg) -> JanusServerMsg -> IO ()
janusEchoCallback chan janusServerMsg = do
  putStrLn $ ("Janus event received (Echo)"::Text) <> show janusServerMsg
  atomically $ writeTChan chan (EchoMsg,janusServerMsg)

janusVoicemailCallback :: JanusServerMsg -> IO ()
janusVoicemailCallback janusServerMsg = do
  putStrLn $ ("Janus event received (VM)"::Text) <> show janusServerMsg

channelLoop :: TChan (MsgSource,JanusServerMsg) -> ServerHandler -> ServerHandler -> IO ()
channelLoop msgChan sipHandler echoHandler = 
    goLoop
  where
    goLoop = do
      msgWithSource <- atomically $ readTChan msgChan
      case msgWithSource of 
        (SipMsg, JanusIncomingCall (Just offer)) -> sendJanusRequest (JanusEchoReq (JanusEchoReqPs True False offer)) echoHandler 
        (EchoMsg, JanusOKEvent (Just answer)) -> do
          sendJanusRequest (JanusAcceptReq answer) sipHandler 
          --threadDelay 1000000
          sendJanusRequest JanusIceConnected sipHandler
        {-
        JanusCallProgressEvent  Accepted (Just answer) -> sendJanusRequest (JanusAcceptReq answer) otherHandler
        JanusIncomingCall (Just offer) -> sendJanusRequest (JanusCallReq (JanusCallReqPs dest offer)) otherHandler 
        --JanusWebRtcUp -> goLoop
        JanusHangupEvent -> sendJanusRequest JanusHangupReq otherHandler  -- exit
        -} 
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
  