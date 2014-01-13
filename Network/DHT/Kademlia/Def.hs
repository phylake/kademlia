module Network.DHT.Kademlia.Def (
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
) where

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

-- | System-wide constant
k :: Int
k = 20

-- | System-wide constant
bits :: Int
bits = 3

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

type NodeId = Integer
type LastSeen = Integer

-- | Sorted by LastSeen
type KBucket = V.Vector (SockAddr, NodeId, LastSeen)

-- | Length of `bits`
type RoutingTable = V.Vector (TVar KBucket)

data RPC = RPC_UNKNOWN
         | RPC_PING
         | RPC_STORE
         | RPC_FIND_NODE NodeId
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
      3 -> return $ RPC_FIND_NODE 0
      4 -> return $ RPC_FIND_VALUE
      otherwise -> return RPC_UNKNOWN
