{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Util.Integral
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashTable.IO as H
import qualified Data.Text as T
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

runKademlia :: Config -> IO ()
runKademlia (Config{..}) = do
  -- init networking
  -- apparently this is also needed on unix without -threaded
  -- http://hackage.haskell.org/package/network-2.4.0.1/docs/Network-Socket.html#v:withSocketsDo
  withSocketsDo $ return ()

  (rt :: RoutingTable) <- atomically defaultRoutingTable

  sock <- socket AF_INET Datagram defaultProtocol
  addr <- inet_addr $ T.unpack cfgHost
  let mySockAddr = SockAddrInet (PortNum $ read $ T.unpack cfgPort) addr
  let thisPeer = Peer (read $ T.unpack cfgNodeId) mySockAddr
  
  mvDataStore <- defaultDataStore >>= newMVar
  mvStoreHT <- H.new >>= newMVar

  {-case args of
    (yourport:[]) -> do
      mv <- newMVar 0
      void . replicateM cthreads . forkIO $ do
        threadDelay $ secToMicro $ read delaySecs
        let yourSockAddr = SockAddrInet (PortNum $ read yourport) addr
        forM_ [0..0] $ \i -> do
          --let bs = BL.toStrict $ encode $ RPC_STORE "c3969900f0616ef5943a" (fromIntegral i) 2 9 "abcd efgh"
          --NB.sendAllTo sock bs yourSockAddr
          --NB.sendAllTo sock "PING" yourSockAddr
          --mv <- newMVar ds
          --rpcStore sock mv testKey $ Peer 0 yourSockAddr
          return ()
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
      loop (KademliaEnv{..}) sock-}
  
  bind sock mySockAddr
  putStrLn $ "Kademlia bound to " ++ T.unpack cfgPort
  caps <- getNumCapabilities
  putStrLn $ "num capabilities [" ++ show caps ++ "]"
  --void $ replicateM (caps - 1) $ forkIO $ loop mvStoreHT sock
  loop (KademliaEnv{..}) sock

  return ()

loop :: KademliaEnv -> Socket -> IO ()
loop (KademliaEnv{..}) sock = forever $ do
  (bs, sockAddr) <- NB.recvFrom sock recvBytes
  forkIO $ do
    let send = flip (NB.sendAllTo sock) sockAddr
    let rpc :: RPC = decode $ BL.fromStrict bs
    case rpc of
      RPC_PING thatPeer -> do
        void $ addPeer thisPeer rt thatPeer
      RPC_STORE k n m l bs -> do
        -- TODO only lock on write?
        storeHT <- takeMVar mvStoreHT -- TAKE
        mtvVec <- H.lookup storeHT k
        tvVec <- case mtvVec of
          Just tvVec -> return tvVec
          Nothing -> do
            tvVec <- atomically $ newTVar $ V.replicate (fromIntegral m) B.empty
            H.insert storeHT k tvVec
            return tvVec
        putMVar mvStoreHT storeHT -- PUT

        mAssm <- atomically $ do
          vec <- readTVar tvVec
          let vec' = vec // [(fromIntegral n, bs)]
          writeTVar tvVec vec'
          return $ tryReassemble vec'
        
        case mAssm of
          Nothing -> return ()
          Just value -> do
            storeHT <- takeMVar mvStoreHT -- TAKE
            H.delete storeHT k
            putMVar mvStoreHT storeHT -- PUT
            
            dataStore@(DataStore{..}) <- takeMVar mvDataStore -- TAKE
            dsSet k value
            putMVar mvDataStore dataStore -- PUT
        return ()
      RPC_FIND_NODE _ -> do
        return ()
      RPC_FIND_VALUE -> do
        return ()
      _ -> do
        putStrLn $ "received: " ++ (BC.unpack bs)
        return ()

rpcStore :: Socket
         -> MVar DataStore
         -> B.ByteString -- ^ key
         -> Peer
         -> IO ()
rpcStore sock mvDataStore key (Peer{..}) = do
  dataStore@(DataStore{..}) <- takeMVar mvDataStore
  mVal <- dsGet key
  putMVar mvDataStore dataStore
  case mVal of
    Nothing -> return ()
    Just v -> do
      -- send first chunk last to optimize tryReassemble
      let (chunk:chunks) = storeChunks key v
      mapM_ send chunks
      send chunk
      -- TODO send PING to update receiving node's k-bucket
  where
    send rpc = NB.sendAllTo sock (BL.toStrict $ encode rpc) location
