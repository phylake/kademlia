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
import           Data.Vector ((!), (//))
import           Data.Word
import           GHC.Conc.Sync (getNumCapabilities)
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
import qualified Data.HashTable.IO as H
import qualified Data.Vector as V
import qualified Network.Socket.ByteString as NB

handler :: MVar Int -> IO ()
handler sigMV = modifyMVar_ sigMV (return . (+1))

cthreads = 1
wait mv = do
  c <- readMVar mv
  if c == cthreads
    then return ()
    else wait mv

runKademlia :: KademliaEnv -> IO ()
runKademlia env = do
  -- init networking
  -- apparently this is also needed on unix without -threaded
  -- http://hackage.haskell.org/package/network-2.4.0.1/docs/Network-Socket.html#v:withSocketsDo
  withSocketsDo $ return ()

  -- "0x" ++ `cat /dev/urandom | tr -cd 'a-f0-9' | head -c 32`

  (rt :: RoutingTable) <- atomically defaultRoutingTable
  --ctx <- zmqContext

  (delaySecs:myport:args) <- getArgs
  
  sock <- socket AF_INET Datagram defaultProtocol
  addr <- inet_addr "127.0.0.1"
  let mySockAddr = SockAddrInet (PortNum $ read myport) addr
  
  case args of
    (yourport:[]) -> do
      mv <- newMVar 0
      void . replicateM cthreads . forkIO $ do
        threadDelay $ secToMicro $ read delaySecs
        let yourSockAddr = SockAddrInet (PortNum $ read yourport) addr
        forM_ [0..1] $ \i -> do
          let bs = BL.toStrict $ encode $ RPC_STORE "c3969900f0616ef5943a" (fromIntegral i) 2 9 "abcd efgh"
          NB.sendAllTo sock bs yourSockAddr
          --NB.sendAllTo sock "PING" yourSockAddr
        c <- takeMVar mv
        putMVar mv $ c+1
      wait mv
    _ -> do
      t <- repeatedTimer
        --(NB.sendAll sock $ BL.toStrict $ encode RPC_PING)
        (return ())
        (sDelay $ read delaySecs)
      
      (storeHT :: StoreHT) <- H.new
      mvStoreHT <- newMVar storeHT

      bind sock mySockAddr
      caps <- getNumCapabilities
      --void $ replicateM (caps - 1) $ forkIO $ loop mvStoreHT sock
      loop mvStoreHT sock

  return ()

type StoreHT = H.BasicHashTable B.ByteString (TVar (V.Vector B.ByteString))
type MVStoreHT = MVar StoreHT

loop :: MVStoreHT -> Socket -> IO ()
loop mvStoreHT sock = forever $ do
  (bs, sockAddr) <- NB.recvFrom sock recvBytes
  forkIO $ do
    let send = flip (NB.sendAllTo sock) sockAddr
    let rpc :: RPC = decode $ BL.fromStrict bs
    case rpc of
      RPC_PING -> do
        --send "PONG"
        putStrLn $ "received: " ++ (show rpc)
      (RPC_STORE k n m l bs) -> do
        storeHT <- takeMVar mvStoreHT -- TAKE MVar
        mtvVec <- H.lookup storeHT k

        tvVec <- case mtvVec of
          Nothing -> do
            tvVec <- atomically $ newTVar $ V.replicate (fromIntegral m) B.empty
            H.insert storeHT k tvVec
            putMVar mvStoreHT storeHT -- PUT MVar
            return tvVec
          Just tvVec -> do
            putMVar mvStoreHT storeHT -- PUT MVar
            return tvVec

        mAssm <- atomically $ do
          vec <- readTVar tvVec
          let vec' = vec // [(fromIntegral n, bs)]
          writeTVar tvVec vec'
          return $ tryReassemble vec'
        
        case mAssm of
          Nothing -> return ()
          Just value -> do
            putStrLn $ "have value [" ++ BC.unpack value ++ "]"
            -- TODO does H.delete need a lock?
            storeHT <- takeMVar mvStoreHT -- TAKE MVar
            H.delete storeHT k
            putMVar mvStoreHT storeHT -- PUT MVar
        return ()
      RPC_FIND_NODE _ -> do
        return ()
      RPC_FIND_VALUE -> do
        return ()
      _ -> do
        putStrLn $ "received: " ++ (BC.unpack bs)
        return ()

-- TODO validate checksum
tryReassemble :: V.Vector B.ByteString -> Maybe B.ByteString
tryReassemble v = if V.any (== B.empty) v
  then Nothing
  else Just $ V.foldl1 B.append v
