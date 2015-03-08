-- | Data and instance definitions
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.DHT.Kademlia.Def (
-- * Data definitions
  KademliaEnv(..)

, DataStore(..)
, defaultDataStore

-- ** Nodes
, NodeId
, Key
, Node(..)

-- ** k-buckets
, KBucket(..)
, defaultKBucket

#ifdef TEST
, LastSeen(..)
#else
, LastSeen
#endif
, lastSeen

-- ** Routing table
, RoutingTable
, defaultRoutingTable
, writeRoutingTable
, readRoutingTable

-- ** Remote procedure calls
, RPC(..)

-- * Constants
, systemK
, systemBits
, systemBytes
, systemAlpha
, recvBytes
, chunkBytes

-- * Configuration
, Config(..)
, DataStoreType(..)
) where
--module Network.DHT.Kademlia.Def where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Aeson as JSON
import           Data.Binary
import           Data.Bits
import           Data.Set
import           Data.Text (Text(..))
import           Data.Time.Clock
import           Data.Vector ((!))
import           GHC.Generics
import           Network.Socket (SockAddr(..), PortNumber(..), Socket(..))
import           System.Directory
import           System.Log.FastLogger
import           Util.Integral
import           Util.Time
import           Util.Words
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashTable.IO as H
import qualified Data.Vector as V
import qualified STMContainers.Map as STM

{-
k-bucket = list = [(IP, Port, NodeId)]
each bucket is length k

160 bits
1 bucket per bit
-}

-- | A system-wide constant for k-bucket length of 20
systemK :: Int
#ifdef TEST
systemK = 2
#else
systemK = 20
#endif

-- | A system-wide constant of 160
systemBits :: Double
#ifdef TEST
systemBits = 3
#else
systemBits = 160
#endif

systemBytes :: Int
systemBytes = fromIntegral $ systemBits/8

-- | Request concurrency (α) during find node and find value operations
systemAlpha :: Int
#ifdef TEST
systemAlpha = 1
#else
systemAlpha = 3
#endif

-- | 548 bytes
-- 
-- > IPb = IPv4 minimum reassembly buffer size = 576 bytes
-- > IPh = IP header = 20 bytes
-- > UDPh = UDP header = 8 bytes
-- > IPb - IPh - UDPh == 548 bytes
recvBytes :: Int
recvBytes = 548

-- | The size in bytes of a chunk of data to be sent across the wire.
-- 
-- >recvBytes - rpc header - key length - sequence number - total chunks
chunkBytes :: Int
#ifdef TEST
chunkBytes = recvBytes - 1 - systemBytes - 4 - 4 - 2
#else
chunkBytes = 2
#endif

-- | Configurable data stores. See 'Config'
data DataStoreType = Hedis -- ^ Redis
                   | HashTables -- ^ In-memory

instance FromJSON DataStoreType where
  parseJSON (Object v) = do
    (t :: Text) <- v .: "type"
    case t of
      "hedis" -> return Hedis
      "hashtables" -> return HashTables
      _ -> mzero
  parseJSON _ = mzero

data Config = Config {
                       cfgThisNode :: Node -- ^ Each node must be given an identity
                     , cfgSeedNode :: Node -- ^ And be seeded with a known node in the network with which to connect
                     , cfgRoutingTablePath :: Text -- ^ The routing table should be persisted in case of node failure
                     , cfgLogDir :: String -- ^ (Optional) Path to log. Default is @\/var\/log\/kademlia@
                     , cfgDSType :: DataStoreType -- (Optional) Defaults to Hedis
                     }

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
    v .: "thisNode" <*>
    v .: "seedNode" <*>
    v .: "routingTablePath" <*>
    (v .:? "logDir" .!= "/var/log/kademlia") <*>
    (v .:? "dataStore" .!= HashTables)
  parseJSON _ = mzero

-- | RPC Store's data chunks keyed on the hash of the data being sent
type StoreHT = H.BasicHashTable B.ByteString (TVar (V.Vector B.ByteString))

data KademliaEnv = KademliaEnv {
                                 dataStore :: DataStore
                               , routingTable :: RoutingTable
                               -- | Transient store for incoming chunks of data for a 'Key'
                               , mvStoreHT :: MVar StoreHT
                               -- | This node's address
                               , thisNode :: Node
                               -- | Outstanding ping requests originating from this node
                               , pingREQs :: TVar (V.Vector (UTCTime, Node))
                               -- | Enables blocking calls to get a list of nodes closest to a key
                               , findNodeRequestors :: STM.Map NodeId (MVar [NodeId])
                               -- | Internal tracking for the FIND_NODE operation
                               , findNodeTracking :: STM.Map NodeId (Set NodeId, Set NodeId, Set NodeId)
                               , logDebug :: (forall a. ToLogStr a => a -> IO ())
                               , logInfo :: (forall a. ToLogStr a => a -> IO ())
                               , logWarn :: (forall a. ToLogStr a => a -> IO ())
                               , logError :: (forall a. ToLogStr a => a -> IO ())
                               --, rpc :: RPCHooks
                               , sock :: Socket
                               }

-- | Flexible key-value storage.
-- 
-- No locking is done on this structure. You must add thread safety yourself.
data DataStore = DataStore {
                           -- | Retrieve a value by key
                             dsGet :: B.ByteString
                                   -> IO (Maybe B.ByteString)
                           -- | Store a key-value pair
                           , dsSet :: B.ByteString
                                   -> B.ByteString
                                   -> IO ()
                           }

-- | Threadsafe 'HashTables'
defaultDataStore :: IO DataStore
defaultDataStore = do
  (ht :: H.BasicHashTable B.ByteString B.ByteString) <- H.new
  mv <- newMVar ht
  return $ DataStore {
    dsSet = (\k v -> withMVar mv $ \ht -> H.insert ht k v)
  , dsGet = (\k -> withMVar mv $ \ht -> H.lookup ht k)
  }

-- | Node ids and keys are both 'systemBits' bit numbers
type NodeId = Key

type Key = Double

-- | Constructor not exposed so there can be no mistakes in units. Use 'lastSeen'
newtype LastSeen = LastSeen { unLast :: Double }
                   deriving (Show, Eq, Generic)

instance ToJSON LastSeen where
instance FromJSON LastSeen where

-- | The only way to construct a 'LastSeen'
lastSeen :: IO LastSeen
lastSeen = liftM LastSeen epochNow

-- | A Kademlia node
data Node = Node {
                   nodeId :: NodeId
                 , location :: SockAddr
                 }
                 deriving (Show, Eq, Generic)

instance ToJSON Node where
instance FromJSON Node where

instance Binary Node where
  put Node{..} = do
    put nodeId
    put location
  get = do
    nodeId <- get
    location <- get
    return Node{..}

instance Binary SockAddr where
  put (SockAddrInet (PortNum portNumber) hostAddress) = do
    put portNumber
    put hostAddress
  get = do
    portNumber <- get
    hostAddress <- get
    return $ SockAddrInet (PortNum portNumber) hostAddress

instance ToJSON PortNumber where
  toJSON (PortNum w) = toJSON w

instance FromJSON PortNumber where
  parseJSON n = liftM PortNum (parseJSON n)

-- TODO flip bytes
instance ToJSON SockAddr where
  toJSON (SockAddrInet port host) = object [
      "port" .= port
    , "host" .= host
    ]
  toJSON _ = Null

-- TODO flip bytes
instance FromJSON SockAddr where
  parseJSON (Object v) = SockAddrInet <$> 
    v .: "port" <*>
    v .: "host"
  parseJSON _ = mzero

-- | A k-bucket is a list of length 'systemK'
-- 
-- The tail is the most recently seen 'Node', head is least recently seen.
-- 
-- "k-buckets effectively implement a least-recently seen eviction policy,
--  except that live nodes are never removed from the list"
data KBucket = KBucket {
                         kContent :: V.Vector (Node, LastSeen) -- ^ Sorted by LastSeen
                       }
                       deriving (Show, Eq, Generic)

-- | Convenience ctor as the 'KBucket' definition changes
defaultKBucket = KBucket {kContent = V.empty}

instance ToJSON KBucket where
instance FromJSON KBucket where

-- TODO document break from bucket split

-- | Length of 'systemBits'
type RoutingTable = V.Vector (TVar KBucket)

-- | Preallocated vector of length 'systemBits' where
-- indices 1 through 'systemBits' - 1 are empty buckets
-- and the bucket at index 0 contains the entire bit range
defaultRoutingTable :: Maybe Int -> STM RoutingTable
defaultRoutingTable mLen = V.replicateM systemBits' (newTVar defaultKBucket)
  where
    systemBits' = maybe (fromIntegral systemBits) id mLen

-- | Write to disk at 'cfgRoutingTablePath' the internal routing table as a JSON
writeRoutingTable :: FilePath -> RoutingTable -> IO ()
writeRoutingTable fp rt = atomically (V.mapM readTVar rt) >>=
  return . JSON.encode . V.takeWhile (/= defaultKBucket) >>=
  BL.writeFile fp

readRoutingTable :: FilePath -> IO RoutingTable
readRoutingTable fp = do
  exists <- doesFileExist fp
  (mBuckets :: Maybe [KBucket]) <- if exists
    then liftM JSON.decode $ BL.readFile fp
    else return Nothing
  case mBuckets of
    Nothing -> atomically $ defaultRoutingTable Nothing
    Just [] -> atomically $ defaultRoutingTable Nothing
    Just buckets -> do
      let remainderLen = Just $ fromIntegral systemBits - length buckets
      atomically $ do
        remainder <- defaultRoutingTable remainderLen
        buckets' <- V.mapM newTVar $ V.fromList buckets
        return $ V.concat [buckets', remainder]

-- | TODO implement lifecycle hooks
data RPCHooks = RPCHooks {
                           foundNode :: Node -> IO ()
                         , ping :: Node -> IO ()
                         }

--data RPCEnvelope = RPCEnvelope NodeId RPC

-- | All remote procedure calls sent between 'Node's in a Kademlia network.
-- 
-- Each RPC has a request (REQ) and response (RES)
-- 
-- Descriptions are from the perspective of the sender
data RPC = RPC_UNKNOWN
         -- | This node
         | RPC_PING_REQ Node
         -- | This node
         | RPC_PING_RES Node
         -- | Key.
         -- Chunk sequence number.
         -- Chunk total.
         -- Chunk length.
         -- Chunk of data.
         | RPC_STORE_REQ B.ByteString -- key
                         Word32 -- chunk sequence number
                         Word32 -- chunk total
                         Word16 -- chunk length
                         B.ByteString -- chunk of data
         -- | This node. The key this node is looking for.
         | RPC_FIND_NODE_REQ Node Key
         -- | This node.
         -- The key some other node is looking for.
         -- The k closest nodes known
         | RPC_FIND_NODE_RES Node Key [NodeId]
         | RPC_FIND_VALUE
         deriving (Show, Eq)

-- TODO NOT Binary
instance Binary RPC where
  put (RPC_PING_REQ node) = do
    putWord8 1
    put node
  put (RPC_PING_RES node) = do
    putWord8 2
    put node
  put (RPC_STORE_REQ k n m l bs) = do
    putWord8 3
    mapM putWord8 $ B.unpack k
    
    -- TODO instance Binary Word32 doesn't work as expected
    putWord8 $ fromIntegral $ n `shiftR` 24
    putWord8 $ fromIntegral $ n `shiftR` 16
    putWord8 $ fromIntegral $ n `shiftR` 8
    putWord8 $ fromIntegral $ n `shiftR` 0
    
    putWord8 $ fromIntegral $ m `shiftR` 24
    putWord8 $ fromIntegral $ m `shiftR` 16
    putWord8 $ fromIntegral $ m `shiftR` 8
    putWord8 $ fromIntegral $ m `shiftR` 0
    
    putWord8 $ fromIntegral $ l `shiftR` 8
    putWord8 $ fromIntegral $ l `shiftR` 0
    
    mapM putWord8 $ B.unpack bs
    return ()
  put (RPC_FIND_NODE_REQ node nodeId) = do
    putWord8 4
    put node
    put nodeId
  put (RPC_FIND_NODE_RES node nodeId nodeIds) = do
    putWord8 5
    put node
    put nodeId
    put nodeIds
  put (RPC_FIND_VALUE) = putWord8 5
  
  get = do
    w <- getWord8
    case w of
      1 -> liftM RPC_PING_REQ get
      2 -> liftM RPC_PING_RES get
      3 -> do
        key <- replicateM systemBytes getWord8 >>= return . B.pack
        (n :: Word32) <- replicateM 4 getWord8 >>= return . toWord32
        (m :: Word32) <- replicateM 4 getWord8 >>= return . toWord32
        (l :: Word16) <- replicateM 2 getWord8 >>= return . toWord16
        chunk <- replicateM (fromIntegral l) getWord8 >>= return . B.pack
        return $ RPC_STORE_REQ key n m l chunk
      4 -> liftM2 RPC_FIND_NODE_REQ get get
      5 -> liftM3 RPC_FIND_NODE_RES get get get
      otherwise -> return RPC_UNKNOWN
