{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.DHT.Kademlia (runKademlia) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Binary
import           Data.Time.Clock
import           Data.Vector ((!), (//))
import           Data.Word
import           GHC.Conc.Sync (getNumCapabilities)
import           Network.DHT.Kademlia.Bucket
import           Network.DHT.Kademlia.Def
import           Network.DHT.Kademlia.Util
import           Network.DHT.Kademlia.Workers
import           Network.Socket hiding (send)
import           System.Environment
import           System.Timeout
import           Util.Integral
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashTable.IO as H
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.Socket.ByteString as NB

runKademlia :: Config -> IO ()
runKademlia config@(Config{..}) = do
  -- init networking
  -- apparently this is also needed on unix without -threaded
  -- http://hackage.haskell.org/package/network-2.4.0.1/docs/Network-Socket.html#v:withSocketsDo
  withSocketsDo $ return ()

  -- TODO bottom of 2.3
  --      1. load periodically saved routing table
  --      2. fall back to table specified in cfgRoutingTablePath for nodes that
  --         aren't new to the network
  --      3. PING each least recently seen node
  (rt :: RoutingTable) <- readRoutingTable $ T.unpack cfgRoutingTablePath

  sock <- socket AF_INET Datagram defaultProtocol

  privateSockAddr <- liftM (SockAddrInet $ PortNum cfgPort) $
                           inet_addr "127.0.0.1"

  publicSockAddr  <- liftM (SockAddrInet $ PortNum cfgPort) $
                           inet_addr $ T.unpack cfgHost

  let thisPeer = Peer (read $ T.unpack cfgNodeId) publicSockAddr

  mvDataStore <- defaultDataStore >>= newMVar
  mvStoreHT <- H.new >>= newMVar
  pingREQs <- atomically $ newTVar V.empty

  let env :: KademliaEnv = KademliaEnv{..}

  -- WORKERS
  interactive env
  pingREQReaper env
  saveRoutingTable env

  -- BIND
  bind sock privateSockAddr
  putStrLn $ "Kademlia bound to " ++ show cfgPort
  caps <- getNumCapabilities
  putStrLn $ "num capabilities [" ++ show caps ++ "]"
  -- TODO see if there are any gains to be had here.
  --      i think recvFrom blocks across all threads making this useless
  --void $ replicateM (caps - 1) $ forkIO $ loop env sock
  loop env

  return ()

loop :: KademliaEnv -> IO ()
loop (KademliaEnv{..}) = forever $ do
  (bs, sockAddr) <- NB.recvFrom sock recvBytes
  forkIO $ do
    let send = flip (NB.sendAllTo sock) sockAddr . BL.toStrict . encode
    let rpc :: RPC = decode $ BL.fromStrict bs
    case rpc of
      RPC_PING_REQ thatPeer -> do
        now <- getCurrentTime
        atomically $ do
          pings <- readTVar pingREQs
          writeTVar pingREQs $ V.snoc pings (now, thatPeer)
        send $ RPC_PING_REP thisPeer
      RPC_PING_REP thatPeer -> do
        foundPeer <- atomically $ do
          pings <- readTVar pingREQs
          let f = (\(bool, v) t@(_, p) -> case bool of
                                            True -> return (True, V.snoc v t)
                                            False -> if p == thatPeer
                                              then return (True, v)
                                              else return (False, V.snoc v t))
          (foundPeer, pings') <- V.foldM f (False, V.empty) pings
          if foundPeer then
            writeTVar pingREQs pings'
          else
            -- nothing to do but write back old value
            writeTVar pingREQs pings
          return foundPeer
        if foundPeer then
          void $ addPeer thisPeer rt thatPeer
        else
          putStrLn "[WARNING] got a PING_REP from an unknown peer"
      RPC_STORE_REQ k n m l bs -> do
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

-- | Store some data on another node by splitting up the data into chunks
rpcStore :: KademliaEnv
         -> Peer -- ^ destination node
         -> B.ByteString -- ^ key
         -> IO ()
rpcStore KademliaEnv{..} Peer{..} key = do
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
      send $ RPC_PING_REQ thisPeer
  where
    send rpc = NB.sendAllTo sock (BL.toStrict $ encode rpc) location
