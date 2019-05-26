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
    [srvIP, portTxt, sipAOR, sipDisplayName, sipPwd, sipAOR2, sipDisplayName2, sipPwd2, sipProxy, dest] ->
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
              regReq2 = JanusRegisterReqPs
                          { userName = pack sipAOR2
                          , displayName = pack sipDisplayName2
                          , password = pack sipPwd2
                          , proxy = pack sipProxy
                          }
          in runTest (pack srvIP) srvPort regReq regReq2 (pack dest)
        Nothing ->
          putStrLn ("Failed to parse sip port (must be Int)"::Text)
    _ -> do putStrLn ("Parametrs: srvIP srvPort sipAOR sipDisplayName sipPwd sipAOR2 sipDisplayName2 sipPwd2 sipProxy dest sipProxy"::Text)        
            putStrLn ("For example: JanusSample janusserver.com 8188 sip:user@sipprovider.com John 123456 sip:user2@sipprovider.com James 654321 sip:sipprovider.com sip:18001234567@sipprovider.com"::Text)        


runTest :: Text -> Int -> JanusRegisterReqPs -> JanusRegisterReqPs -> Text -> IO ()
runTest srvIP port janusRegisterReqPs janusRegisterReqPs2 dest = do
  setupLog
  connectorHandle <- initConnector srvIP port
  waitForConnectivity connectorHandle
  msgChan <- newTChanIO   
  srvHandlerMaybe <- createHandler connectorHandle Sip (janusSipCallback msgChan 0)
  srvHandlerMaybe2 <- createHandler connectorHandle Sip (janusSipCallback msgChan 1) 
  case (srvHandlerMaybe, srvHandlerMaybe2) of
    (Just sipHandler, Just sipHandler2) -> do
      sendJanusRequest (JanusRegisterReq janusRegisterReqPs) sipHandler  
      sendJanusRequest (JanusRegisterReq janusRegisterReqPs2) sipHandler2  
      channelLoop msgChan sipHandler sipHandler2 dest
      putStrLn ("Press [Enter] to exit"::Text)
      void getLine
    _ -> putStrLn ("Failed to create some handlers"::Text)
   
janusSipCallback :: TChan (Int,JanusServerMsg) -> Int -> JanusServerMsg -> IO ()
janusSipCallback chan num janusServerMsg = do
  putStrLn $ ("Janus event received (SIP)/"::Text) <> show num <> ("-->"::Text) <> show janusServerMsg
  atomically $ writeTChan chan (num,janusServerMsg)
  
janusEchoCallback :: JanusServerMsg -> IO ()
janusEchoCallback janusServerMsg = do
  putStrLn $ ("Janus event received (Echo)"::Text) <> show janusServerMsg

janusVoicemailCallback :: JanusServerMsg -> IO ()
janusVoicemailCallback janusServerMsg = do
  putStrLn $ ("Janus event received (VM)"::Text) <> show janusServerMsg

channelLoop :: TChan (Int,JanusServerMsg) -> ServerHandler -> ServerHandler -> Text -> IO ()
channelLoop msgChan sipHandler sipHandler2 dest = 
    goLoop
  where
    goLoop = do
      (num, msg) <- atomically $ readTChan msgChan
      let otherHandler = if num==0 then sipHandler2 else sipHandler
      case msg of 
        JanusCallProgressEvent  Accepted (Just answer) -> sendJanusRequest (JanusAcceptReq answer) otherHandler
        JanusIncomingCall (Just offer) -> sendJanusRequest (JanusCallReq (JanusCallReqPs dest offer)) otherHandler 
        --JanusWebRtcUp -> goLoop
        JanusHangupEvent -> sendJanusRequest JanusHangupReq otherHandler  -- exit
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
  