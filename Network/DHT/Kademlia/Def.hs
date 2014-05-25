{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.DHT.Kademlia.Def (
  systemK
, systemBits
, systemBytes
, recvBytes
, chunkBytes

, DataStoreType(..)
, Config(..)

, KademliaEnv(..)

, DataStore(..)
, defaultDataStore

, NodeId
, Key

#ifdef TEST
, LastSeen(..)
#else
, LastSeen
#endif
, lastSeen

, Peer(..)

, KBucket(..)
, defaultKBucket

, RoutingTable
, defaultRoutingTable
, writeRoutingTable
, readRoutingTable

, RPC(RPC_PING_REQ,
      RPC_PING_REP,
      RPC_STORE_REQ,
      RPC_FIND_NODE,
      RPC_FOUND_NODE,
      RPC_FIND_VALUE)
) where
--module Network.DHT.Kademlia.Def where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Aeson as JSON
import           Data.Binary
import           Data.Bits
import           Data.Text (Text(..))
import           Data.Time.Clock
import           Data.Vector ((!))
import           GHC.Generics
import           Network.Socket (SockAddr(..), PortNumber(..), Socket(..))
import           System.Directory
import           Util.Integral
import           Util.Time
import           Util.Words
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashTable.IO as H
import qualified Data.Vector as V

{-
k-bucket = list = [(IP, Port, NodeId)]
each bucket is length k

160 bits
1 bucket per bit
-}

systemK :: Int
#ifdef TEST
systemK = 2
#else
systemK = 20
#endif

systemBits :: Double
#ifdef TEST
systemBits = 3
#else
systemBits = 160
#endif

systemBytes :: Int
systemBytes = fromIntegral $ systemBits/8

-- | IPv4 minimum reassembly buffer size = 576 bytes
-- minus IP header = 20 bytes
-- minus UDP header = 8 bytes
-- == 548 bytes
recvBytes :: Int
recvBytes = 548

-- | >   recvBytes
-- | > - rpc header
-- | > - key length
-- | > - sequence number
-- | > - total chunks
chunkBytes :: Int
chunkBytes = recvBytes - 1 - systemBytes - 4 - 4 - 2

data DataStoreType = Hedis
                   | HashTables

instance FromJSON DataStoreType where
  parseJSON (Object v) = do
    (t :: Text) <- v .: "type"
    case t of
      "hedis" -> return Hedis
      "hashtables" -> return HashTables
      _ -> mzero
  parseJSON _ = mzero

data Config = Config {
                       cfgThisNode :: Peer
                     , cfgSeedNode :: Peer
                     , cfgRoutingTablePath :: Text
                     , cfgDSType :: DataStoreType
                     }

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
    v .: "thisNode" <*>
    v .: "seedNode" <*>
    v .: "routingTablePath" <*>
    (v .:? "dataStore" .!= HashTables)
  parseJSON _ = mzero

-- | RPC Store's data chunks
type StoreHT = H.BasicHashTable B.ByteString (TVar (V.Vector B.ByteString))

data KademliaEnv = KademliaEnv {
                                 config :: Config
                               , dataStore :: DataStore
                               , rt :: RoutingTable
                               , mvStoreHT :: MVar StoreHT
                               , thisPeer :: Peer -- ^ this node's address
                               , pingREQs :: TVar (V.Vector (UTCTime, Peer)) -- ^ outstanding ping requests originating from this node
                               , sock :: Socket
                               --, rpc :: RPCHooks
                               }

-- | This is not thread safe you must add thread safety yourself
data DataStore = DataStore {
                             dsGet :: B.ByteString -- ^ key
                                   -> IO (Maybe B.ByteString) -- ^ maybe value
                           , dsSet :: B.ByteString -- ^ key
                                   -> B.ByteString -- ^ value
                                   -> IO ()
                           }

defaultDataStore :: IO DataStore
defaultDataStore = do
  (ht :: H.BasicHashTable B.ByteString B.ByteString) <- H.new
  mv <- newMVar ht
  return $ DataStore {
    dsSet = (\k v -> withMVar mv $ \ht -> H.insert ht k v)
  , dsGet = (\k -> withMVar mv $ \ht -> H.lookup ht k)
  }

-- | Node ids and keys are synonymous
type NodeId = Key

-- | A key is an integer containing `systemBits` bits
type Key = Double

-- | Constructor not exposed so there can be no mistakes in units
newtype LastSeen = LastSeen { unLast :: Double }
                   deriving (Show, Eq, Generic)

instance ToJSON LastSeen where
instance FromJSON LastSeen where

lastSeen :: IO LastSeen
lastSeen = liftM LastSeen epochNow

data Peer = Peer {
                   nodeId :: NodeId
                 , location :: SockAddr
                 }
                 deriving (Show, Eq, Generic)

instance ToJSON Peer where
instance FromJSON Peer where

instance Binary Peer where
  put (Peer{..}) = do
    put nodeId
    put location
  get = do
    nodeId <- get
    location <- get
    return Peer{..}

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

-- | range is [kMinRange, kMaxRange)
-- 
-- tail is most recently seen, head is least recently seen
-- 
-- "k-buckets effectively implement a least-recently seen eviction policy,
--  except that live nodes are never removed from the list"
data KBucket = KBucket {
                         -- TODO how is lock optimism affected for reads on Peer
                         --      when there are frequent writes to LastSeen?
                         kContent :: V.Vector (Peer, LastSeen) -- ^ Sorted by LastSeen
                       , kMinRange :: Double
                       , kMaxRange :: Double
                       }
                       deriving (Show, Eq, Generic)

defaultKBucket = KBucket {kContent = V.empty, kMinRange = 0, kMaxRange = 0}

instance ToJSON KBucket where
instance FromJSON KBucket where

-- | Length of `systemBits`
type RoutingTable = V.Vector (TVar KBucket)

-- | Preallocated vector of length systemBits where
-- indices 1 through systemBits - 1 are empty buckets that will contain contents
-- of other buckets as they're split,
-- and the bucket at index 0 contains the entire bit range
defaultRoutingTable :: Maybe Int -> STM RoutingTable
defaultRoutingTable mLen = do
  rt <- V.replicateM systemBits' (newTVar defaultKBucket)
  writeTVar (rt ! 0) $ defaultKBucket {kMaxRange = 2 ** systemBits}
  return rt
  where
    systemBits' = maybe (fromIntegral systemBits) id mLen

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
      

data RPCHooks = RPCHooks {
                           foundNode :: Peer -> IO ()
                         , ping :: Peer -> IO ()
                         }

--data RPCEnvelope = RPCEnvelope NodeId RPC

data RPC = RPC_UNKNOWN
         | RPC_PING_REQ Peer
         | RPC_PING_REP Peer
         | RPC_STORE_REQ B.ByteString -- ^ key
                         Word32 -- ^ chunk sequence number
                         Word32 -- ^ chunk total
                         Word16 -- ^ chunk length
                         B.ByteString -- ^ chunk of data
         | RPC_FIND_NODE Peer
         | RPC_FOUND_NODE Peer
         | RPC_FIND_VALUE
         deriving (Show, Eq)

instance Binary RPC where
  put (RPC_PING_REQ peer) = do
    putWord8 1
    put peer
  put (RPC_PING_REP peer) = do
    putWord8 2
    put peer
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
  put (RPC_FIND_NODE _) = putWord8 4
  put (RPC_FIND_VALUE) = putWord8 5
  
  get = do
    w <- getWord8
    case w of
      1 -> liftM RPC_PING_REQ get
      2 -> liftM RPC_PING_REP get
      3 -> do
        key <- replicateM systemBytes getWord8 >>= return . B.pack
        (n :: Word32) <- replicateM 4 getWord8 >>= return . toWord32
        (m :: Word32) <- replicateM 4 getWord8 >>= return . toWord32
        (l :: Word16) <- replicateM 2 getWord8 >>= return . toWord16
        chunk <- replicateM (fromIntegral l) getWord8 >>= return . B.pack
        return $ RPC_STORE_REQ key n m l chunk
      --4 -> return $ RPC_FIND_NODE $ Peer 0 
      5 -> return $ RPC_FIND_VALUE
      otherwise -> return RPC_UNKNOWN
