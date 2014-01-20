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

import           Control.Concurrent.STM (TVar(..))
import           Data.Binary
import           Network.Socket (SockAddr(..))
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

data KBucket = KBucket {
                         kContent :: V.Vector (Peer, LastSeen) -- ^ Sorted by LastSeen
                       , kMinRange :: Double
                       , kMaxRange :: Double
                       }
                       deriving (Show, Eq)

defaultKBucket = KBucket {kContent = V.empty, kMinRange = 0, kMaxRange = 2 ** systemBits}

-- | Length of `bits`
--
-- Outer TVar is only contentious on k-bucket splits.
type RoutingTable = TVar (V.Vector (TVar KBucket))

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
