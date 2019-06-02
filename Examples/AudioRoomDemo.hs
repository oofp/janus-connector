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
  putStrLn ("Welcome to Janus sip audio demo app "::Text)
  args <- getArgs
  case args of 
    [srvIP, portTxt, sipAOR, sipDisplayName, sipPwd, sipProxy, roomTxt, display] ->
      let srvPortMaybe :: Maybe Int
          srvPortMaybe = readMaybe portTxt
          roomMaybe = readMaybe roomTxt
      in case (srvPortMaybe, roomMaybe) of
        (Just srvPort, Just room) -> 
          let regReq = JanusRegisterReqPs
                          { userName = pack sipAOR
                          , displayName = pack sipDisplayName
                          , password = pack sipPwd
                          , proxy = pack sipProxy
                          }
          in runTest (pack srvIP) srvPort regReq room (pack display)
        _ ->
          putStrLn ("Failed to parse sip port or room (must be Int)"::Text)
    _ -> do putStrLn ("Parametrs: srvIP srvPort sipAOR sipDisplayName sipPwd sipProxy room demo"::Text)        
            putStrLn ("For example: SipEchoDemo janusserver.com 8188 sip:user@sipprovider.com John 123456 sip:sipprovider.com room1 John"::Text)        


runTest :: Text -> Int -> JanusRegisterReqPs -> Int -> Text -> IO ()
runTest srvIP port janusRegisterReqPs room display = do
  setupLog
  connectorHandle <- initConnector srvIP port
  waitForConnectivity connectorHandle
  msgChan <- newTChanIO   
  srvHandlerMaybe <- createHandler connectorHandle Sip (janusSipCallback msgChan)
  arHandlerMaybe <- createHandler connectorHandle AudioBridge (janusAudioRoomCallback msgChan) 
  case (srvHandlerMaybe, arHandlerMaybe) of
    (Just sipHandler, Just arHandler) -> do
      sendJanusRequest (JanusRegisterReq janusRegisterReqPs) sipHandler  
      channelLoop msgChan sipHandler arHandler room display
      putStrLn ("Press [Enter] to exit"::Text)
      void getLine
    _ -> putStrLn ("Failed to create some handlers"::Text)

data MsgSource 
  = SipMsg
  | ARMsg

janusSipCallback :: TChan (MsgSource,JanusServerMsg) -> JanusServerMsg -> IO ()
janusSipCallback chan janusServerMsg = do
  putStrLn $ ("Janus event received (SIP)/"::Text) <> ("-->"::Text) <> show janusServerMsg
  atomically $ writeTChan chan (SipMsg,janusServerMsg)
  
janusAudioRoomCallback :: TChan (MsgSource,JanusServerMsg) -> JanusServerMsg -> IO ()
janusAudioRoomCallback chan janusServerMsg = do
  putStrLn $ ("Janus event received (AR)"::Text) <> show janusServerMsg
  atomically $ writeTChan chan (ARMsg,janusServerMsg)

channelLoop :: TChan (MsgSource,JanusServerMsg) -> ServerHandler -> ServerHandler -> Int -> Text -> IO ()
channelLoop msgChan sipHandler arHandler room display = 
    goLoop
  where
    goLoop = do
      msgWithSource <- atomically $ readTChan msgChan
      case msgWithSource of 
        (SipMsg, JanusIncomingCall (Just offer)) -> do
          sendJanusRequest (JanusAudioRoomJoinReq (JanusAudioRoomJoinReqPs room display)) arHandler
          sendJanusRequest (JanusAudioRoomConfigureReq (JanusAudioRoomConfigureReqPs False offer)) arHandler 
        (ARMsg, JanusOKEvent (Just answer)) -> do
          sendJanusRequest (JanusAcceptReq answer) sipHandler 
          sendJanusRequest JanusIceConnected arHandler
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
  