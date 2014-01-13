{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Network.DHT.Kademlia (runKademlia) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Suspend.Lifted (sDelay)
import           Control.Concurrent.Timer
import           Control.Monad
import           Data.Binary
import           Data.Vector ((!))
import           Data.Word
import           Network.DHT.Kademlia.Bucket
import           Network.DHT.Kademlia.Def
import           Network.DHT.Kademlia.Util
import           Network.Socket
import           System.Environment
import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import           System.Timeout
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Network.Socket.ByteString as NB

handler :: MVar Int -> IO ()
handler sigMV = modifyMVar_ sigMV (return . (+1))

runKademlia :: DataStore -> IO ()
runKademlia ds = do
  (rt :: RoutingTable) <- V.replicateM systemBits (atomically $ newTVar defaultKBucket)
                      >>= atomically . newTVar

  (delaySecs:myport:args) <- getArgs
  
  sock <- socket AF_INET Datagram defaultProtocol
  addr <- inet_addr "127.0.0.1"
  let mySockAddr = SockAddrInet (PortNum $ read myport) addr
  
  case args of
    (yourport:[]) -> void $ replicateM 20 $ forkIO $ do
      threadDelay $ secToMicro $ read delaySecs
      let yourSockAddr = SockAddrInet (PortNum $ read yourport) addr
      NB.sendAllTo sock (BL.toStrict $ encode RPC_PING) yourSockAddr
    _ -> return ()
  
  t <- repeatedTimer
    --(NB.sendAll sock $ BL.toStrict $ encode RPC_PING)
    (return ())
    (sDelay $ read delaySecs)
  
  {-sigMV <- newMVar 0
  installHandler sigINT (Catch $ handler sigMV) Nothing
  installHandler sigTERM (Catch $ handler sigMV) Nothing-}

  bind sock mySockAddr
  forever $ do
    (bs, sockAddr) <- NB.recvFrom sock recvBytes
    forkIO $ do
      let send = flip (NB.sendAllTo sock) sockAddr
      let rpc :: RPC = decode $ BL.fromStrict bs
      case rpc of
        RPC_PING -> do
          send "PONG"
          putStrLn $ "received: " ++ (show rpc)
        RPC_STORE -> do
          return ()
        RPC_FIND_NODE _ -> do
          return ()
        RPC_FIND_VALUE -> do
          return ()
        _ -> do
          putStrLn $ "received: " ++ (BC.unpack bs)
          return ()

  let cleanup = do {
    putStrLn "cleaning up";
    close sock;
    stopTimer t;
  }
    
  --loop sock sigMV cleanup
  return ()

loop :: Socket -> MVar Int -> IO () -> IO ()
loop sock sigMV cleanup = do
  return ()
  --mData <- timeout (secToMicro 4) $ B.recvFrom sock 0x200
