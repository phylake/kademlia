{-# LANGUAGE CPP #-}
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

import           Control.Concurrent.STM
import           Data.Binary
import           Data.Vector ((!))
import           Network.Socket (SockAddr(..))
import           Util.Integral
import qualified Data.ByteString as B
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

-- | IPv4 minimum reassembly buffer size = 576 bytes
-- minus IP header = 20 bytes
-- minus UDP header = 8 bytes
-- == 548 bytes
recvBytes :: Int
recvBytes = 548

data Hooks = Hooks {
                     hkPing :: (Int -> IO ())
                   , hkPing2 :: (Int -> IO ())
                   }

type DataStore = (B.ByteString -> IO (Maybe B.ByteString))

type Key = Double
type LastSeen = Double

data Peer = Peer {
                   nodeId :: Double
                 , location :: SockAddr
                 }
                 deriving (Show, Eq)

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

defaultRoutingTable :: STM RoutingTable
defaultRoutingTable = do
  rt <- V.replicateM systemBits' (newTVar defaultKBucket)
  writeTVar (rt ! 0) $ defaultKBucket {kMaxRange = 2 ** systemBits}
  return rt
  where
    systemBits' = fromIntegral systemBits

data RPC = RPC_UNKNOWN
         | RPC_PING
         | RPC_STORE
         | RPC_FIND_NODE Peer
         | RPC_FIND_VALUE
         deriving (Show)

instance Binary RPC where
  put (RPC_PING) = putWord8 1
  put (RPC_STORE) = putWord8 2
  put (RPC_FIND_NODE _) = putWord8 3
  put (RPC_FIND_VALUE) = putWord8 4
  
  get = do
    w <- getWord8
    case w of
      1 -> return $ RPC_PING
      2 -> return $ RPC_STORE
      --3 -> return $ RPC_FIND_NODE $ Peer 0 
      4 -> return $ RPC_FIND_VALUE
      otherwise -> return RPC_UNKNOWN
