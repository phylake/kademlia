{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
#ifdef TEST
module Network.DHT.Kademlia (receiveRPC, rpcStore) where
#else
module Network.DHT.Kademlia (runKademlia) where
#endif

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Binary
import           Data.Monoid
import           Data.Time.Clock
import           Data.Vector ((//))
import           Network.DHT.Kademlia.Bucket
import           Network.DHT.Kademlia.Def
import           Network.DHT.Kademlia.Util
import           Network.DHT.Kademlia.Workers
import           Network.Socket hiding (send)
import           System.FilePath ((</>))
import           System.Log.FastLogger
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashTable.IO as H
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.Socket.ByteString as NB
import qualified STMContainers.Map as STM

-- | Kademlia as a library starts here.
-- 
-- If you've installed kademlia type @kademlia@ to get help running it.
-- 
-- Also refer to the <https://github.com/phylake/kademlia getting started documentation>
runKademlia :: Config -> IO ()
runKademlia config@Config{..} = do
  -- init networking
  -- apparently this is also needed on unix if -threaded is absent
  -- http://hackage.haskell.org/package/network-2.4.0.1/docs/Network-Socket.html#v:withSocketsDo
  withSocketsDo $ return ()

  -- BEGIN env
  sock <- socket AF_INET Datagram defaultProtocol

  let thisNode@(Node thisNodeId (SockAddrInet cfgPort cfgHost)) = cfgThisNode
  --putStrLn $ show cfgThisNode
  
  privateSockAddr <- liftM (SockAddrInet cfgPort) $
                           inet_addr "127.0.0.1"

  dataStore <- defaultDataStore -- TODO use cfgDSType
  mvStoreHT <- H.new >>= newMVar
  pingREQs <- atomically $ newTVar V.empty
  routingTable <- readRoutingTable $ T.unpack cfgRoutingTablePath

#ifdef DEBUG
  logDebugSet <- newStdoutLoggerSet defaultBufSize
  logInfoSet <- newStdoutLoggerSet defaultBufSize
  logWarnSet <- newStdoutLoggerSet defaultBufSize
  logErrorSet <- newStdoutLoggerSet defaultBufSize
#else
  logInfoSet <- newFileLoggerSet defaultBufSize $ cfgLogDir </> "info.log"
  logWarnSet <- newFileLoggerSet defaultBufSize $ cfgLogDir </> "warn.log"
  logErrorSet <- newFileLoggerSet defaultBufSize $ cfgLogDir </> "error.log"
#endif
  forkIO $ forever $ do
    threadDelay 1000000 -- 1s
    flushLogStr logInfoSet
    flushLogStr logWarnSet
    flushLogStr logErrorSet
#ifdef DEBUG
    flushLogStr logDebugSet
  let logDebug str = pushLogStr logInfoSet $ toLogStr str <> "\n"
#else
  let logDebug _ = return ()
#endif
  let logInfo str = pushLogStr logInfoSet $ toLogStr str <> "\n"
  let logWarn str = pushLogStr logWarnSet $ toLogStr str <> "\n"
  let logError str = pushLogStr logErrorSet $ toLogStr str <> "\n"

  let env = KademliaEnv{..}
  -- END env

  -- WORKERS
  interactive env
  pingREQReaper env
  persistRoutingTable env config

  -- BIND
  logDebug $ "privateSockAddr " ++ show privateSockAddr
  bind sock privateSockAddr
  logInfo $ "Kademlia bound to " ++ show cfgPort
  caps <- getNumCapabilities
  logInfo $ "num capabilities [" ++ show caps ++ "]"
  
  -- JOIN network
  let send = flip (NB.sendAllTo sock) (location cfgSeedNode) . BL.toStrict . encode
  joinNetwork env send cfgSeedNode

  -- MAIN LOOP
  loop env

  -- TODO see if there are any gains to be had here.
  --      i think recvFrom blocks across all threads making this useless
  --void $ replicateM caps $ forkIO $ loop env
  
  return ()

loop :: KademliaEnv -> IO ()
loop env = forever $ do
  (bs, sockAddr) <- NB.recvFrom (sock env) recvBytes
  forkIO $ do
    let send = flip (NB.sendAllTo $ sock env) sockAddr . BL.toStrict . encode
    receiveRPC env send $ decode $ BL.fromStrict bs

receiveRPC :: KademliaEnv
           -> (RPC -> IO ()) -- ^ send outbound RPCs
           -> RPC -- ^ inbound RPC
           -> IO ()
receiveRPC KademliaEnv{..} send (RPC_PING_REQ thatNode) = do
  now <- getCurrentTime
  atomically $ do
    pings <- readTVar pingREQs
    writeTVar pingREQs $ V.snoc pings (now, thatNode)
  send $ RPC_PING_RES thisNode

receiveRPC KademliaEnv{..} send (RPC_PING_RES thatNode) = do
  foundNode <- atomically $ do
    pings <- readTVar pingREQs
    -- walk the list of outstanding pings and prune out thatNode if found
    case prunePings pings thatNode of
      Nothing -> return False
      Just pings' -> do
        writeTVar pingREQs pings'
        return True  
  if foundNode then
    void $ addNode thisNode routingTable thatNode
  else
    logWarn ("got a PING_REP from an unknown node" :: BC.ByteString)
  
receiveRPC KademliaEnv{..} send (RPC_STORE_REQ k n m _ bs) = do
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

      -- store key-value pair
      (dsSet dataStore) k value
  return ()

receiveRPC KademliaEnv{..} send (RPC_FIND_NODE_REQ node _) = do
  addNode thisNode routingTable node >>= either logError (\_ -> return ())
  return ()

receiveRPC KademliaEnv{..} send rpc = do
  logError $ "unimplemented: " ++ show rpc
  return ()


-- | Store some data on another node
rpcStore :: KademliaEnv
         -> (RPC -> IO ()) -- ^ send outbound RPCs
         -> B.ByteString -- ^ key
         -> IO ()
rpcStore KademliaEnv{..} send key = do
  mVal <- (dsGet dataStore) key
  case mVal of
    Nothing -> return ()
    Just v -> do
      -- send first chunk last to optimize tryReassemble
      let (chunk:chunks) = storeChunks key v
      mapM_ send chunks
      send chunk
      send $ RPC_PING_REQ thisNode

-- TODO exported blocking findNode that uses findNodeRequestors

rpcFindNode :: KademliaEnv
         -> (RPC -> IO ()) -- ^ send outbound RPCs
         -> NodeId
         -> IO ()
rpcFindNode KademliaEnv{..} send nodeId = do
  -- TODO track 3 sets
  --      1. the nodes we've sent     RPC_FIND_NODE_REQ to so they're not resent
  --      2. the nodes we've received RPC_FIND_NODE_RES from so they can be
  --         removed from our k-bucket if they timeout
  --      3. the systemK best nodes we've heard about from RPC_FIND_NODE_REQ
  -- stop when we hear back from all requests that haven't timed out &&
  -- no better nodes are found
  closest <- kClosestNodes nodeId routingTable
  replicateM systemAlpha $ send $ RPC_FIND_NODE_REQ thisNode nodeId
  return ()

-- | Bottom of 2.3
-- 
-- "To join a network, a node u must have a contact to an already participating
--  node w. u inserts w into the appropriate k-bucket. u then performs a node
--  lookup for its own node ID. Finally, u refreshes all k-buckets further away
--  than its closest neighbor. During the refreshes, u both populates its own
--  k-buckets and inserts itself into other nodes' k-buckets as necessary"
joinNetwork :: KademliaEnv
            -> (RPC -> IO ()) -- ^ send outbound RPCs
            -> Node
            -> IO ()
joinNetwork KademliaEnv{..} send seed@Node{..} = do
  send $ RPC_PING_REQ seed
