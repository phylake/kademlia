{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-module Network.DHT.Kademlia.Def (
  k
, bits
, recvBytes
, Hooks
, DataStore
, NodeId
, LastSeen
, KBucket
, RoutingTable
, RPC(RPC_PING, RPC_STORE, RPC_FIND_NODE, RPC_FIND_VALUE)
) where-}
module Network.DHT.Kademlia.Def where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Aeson
import           Data.Binary
import           Data.Bits
import           Data.Text (Text(..))
import           Data.Vector ((!))
import           Network.Socket (SockAddr(..), PortNumber(..))
import           Util.Integral
import           Util.Words
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.HashTable.IO as H

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

type StoreHT = H.BasicHashTable B.ByteString (TVar (V.Vector B.ByteString))
type MVStoreHT = MVar StoreHT

data KademliaEnv = KademliaEnv {
                                 mvDataStore :: MVar DataStore
                               , rt :: RoutingTable
                               , mvStoreHT :: MVar StoreHT
                               , thisPeer :: Peer
                               --, rpc :: RPCHooks
                               }

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
  return $ DataStore {
    dsSet = H.insert ht
  , dsGet = H.lookup ht
  }

type NodeId = Key
type Key = Double
type LastSeen = Double

data Peer = Peer {
                   nodeId :: NodeId
                 , location :: SockAddr
                 }
                 deriving (Show, Eq)

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

-- | range is [kMinRange, kMaxRange)
data KBucket = KBucket {
                         -- TODO how is lock optimism affected for reads on Peer
                         --      when there are frequent writes to LastSeen
                         kContent :: V.Vector (Peer, LastSeen) -- ^ Sorted by LastSeen
                       , kMinRange :: Double
                       , kMaxRange :: Double
                       }
                       deriving (Show, Eq)

defaultKBucket = KBucket {kContent = V.empty, kMinRange = 0, kMaxRange = 0}

-- | Length of `systemBits`
type RoutingTable = V.Vector (TVar KBucket)

-- | Preallocated vector of length systemBits where
-- indices 1 through systemBits - 1 are buckets needing configured to split into
-- and the bucket at index 0 contains the entire bit range
defaultRoutingTable :: STM RoutingTable
defaultRoutingTable = do
  rt <- V.replicateM systemBits' (newTVar defaultKBucket)
  writeTVar (rt ! 0) $ defaultKBucket {kMaxRange = 2 ** systemBits}
  return rt
  where
    systemBits' = fromIntegral systemBits

data RPCHooks = RPCHooks {
                           foundNode :: Peer -> IO ()
                         , ping :: Peer -> IO ()
                         }

data RPCEnvelope = RPCEnvelope NodeId RPC

data RPC = RPC_UNKNOWN
         | RPC_PING Peer
         | RPC_STORE B.ByteString -- ^ key
                     Word32 -- ^ sequence number
                     Word32 -- ^ total chunks
                     Word16 -- ^ length of chunk to come
                     B.ByteString -- ^ chunk of data
         | RPC_FIND_NODE Peer
         | RPC_FOUND_NODE Peer
         | RPC_FIND_VALUE
         deriving (Show, Eq)

instance Binary RPC where
  put (RPC_PING peer) = do
    putWord8 1
    put peer
  put (RPC_STORE k n m l bs) = do
    putWord8 2
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
  put (RPC_FIND_NODE _) = putWord8 3
  put (RPC_FIND_VALUE) = putWord8 4
  
  get = do
    w <- getWord8
    case w of
      1 -> liftM RPC_PING get
      2 -> do
        key <- replicateM systemBytes getWord8 >>= return . B.pack
        (n :: Word32) <- replicateM 4 getWord8 >>= return . toWord32
        (m :: Word32) <- replicateM 4 getWord8 >>= return . toWord32
        (l :: Word16) <- replicateM 2 getWord8 >>= return . toWord16
        chunk <- replicateM (fromIntegral l) getWord8 >>= return . B.pack
        return $ RPC_STORE key n m l chunk
      --3 -> return $ RPC_FIND_NODE $ Peer 0 
      4 -> return $ RPC_FIND_VALUE
      otherwise -> return RPC_UNKNOWN

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
                       cfgNodeId :: Text
                     , cfgHost :: Text
                     , cfgPort :: Text
                     , cfgDSType :: DataStoreType
                     }

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
    v .: "nodeId" <*>
    (v .:? "host" .!= "127.0.0.1") <*>
    (v .:? "port" .!= "3000") <*>
    (v .:? "dataStore" .!= HashTables)
  parseJSON _ = mzero
