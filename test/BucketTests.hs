module BucketTests (kBucket, routingTable) where

import           Control.Concurrent.STM
import           Control.Monad (liftM2)
import           Data.Vector ((!), (//))
import           Network.DHT.Kademlia.Bucket
import           Network.DHT.Kademlia.Def
import           Network.Socket (SockAddr(SockAddrUnix))
import           Test.Hspec
import           TestEq
import qualified Data.Vector as V

kBucket :: Spec
kBucket = describe "k-bucket" $ do
  it "splits range in [kMinRange, kMaxRange)" $
    splitKBucketImpl fullKBucket `shouldBe` (leftKBucket, rightKBucket)

routingTable :: Spec
routingTable = describe "routing table" $ do
  describe "full bucket behavior" $ do
    it "splits at index 0 into 2 buckets at indices 0 and 1" $
      (rtOneFullBucket >>= split) `shouldReturn` (leftKBucket, rightKBucket)
    it "splits when adding a peer and the peer's id is in this node's range" $
      (rtOneFullBucket >>= addPeerInRange newPeer) `shouldReturn` True
    it "drops peer when bucket is full and peer's id is not in this node's range" $
      addPeerDrop `shouldReturn` True

newPeer :: Peer
newPeer = defaultPeer {nodeId = 7} -- fullBucket range is 0-7

thisNode :: Peer
thisNode = defaultPeer {nodeId = 1}

-- the single bucket split and the newPeer exists in it
splitBucket = rightKBucket {
  kContent = V.fromList [(newPeer, LastSeen 0)]
}

addPeerInRange :: Peer -> RoutingTable -> IO Bool
addPeerInRange thatNode rt = do
  e <- addPeer thisNode rt thatNode
  case e of
    Left _ -> return False
    Right _ -> do
      rt' <- stripSTM rt
      return $ leftKBucket ~= (rt' ! 0)
            && splitBucket ~= (rt' ! 1)

splitDropBucket = rightKBucket {
  kContent = V.fromList [
    (defaultPeer {nodeId = 6}, LastSeen 0)
  , (defaultPeer {nodeId = 7}, LastSeen 0)
  ]
}

addPeerDrop :: IO Bool
addPeerDrop = do
  rt <- rtOneFullBucket
  
  -- cause a split and fill second k-bucket
  addPeer thisNode rt $ fst $ (kContent splitDropBucket ! 0)
  addPeer thisNode rt $ fst $ (kContent splitDropBucket ! 1)
  -- second k-bucket is now full
  
  -- ignore new node 5 in second bucket [4,8)
  e <- addPeer thisNode rt $ defaultPeer {nodeId = 5}
  case e of
    Left _ -> return False
    Right _ -> do
      rt' <- stripSTM rt
      return $     leftKBucket ~= (rt' ! 0)
            && splitDropBucket ~= (rt' ! 1)

split :: RoutingTable -> IO (KBucket, KBucket)
split rt = atomically $ do
  splitKBucket 0 rt
  liftM2 (,)
    (readTVar $ rt ! 0)
    (readTVar $ rt ! 1)

stripSTM :: RoutingTable -> IO (V.Vector KBucket)
stripSTM = V.mapM (atomically . readTVar)

rtOneFullBucket :: IO RoutingTable
rtOneFullBucket = atomically $ do
  rt <- defaultRoutingTable
  writeTVar (rt ! 0) fullKBucket
  return rt

fullKBucket = KBucket {
  kContent = fullContent
, kMinRange = 0
, kMaxRange = 2 ** systemBits
}
leftKBucket = KBucket {
  kContent = fullContent
, kMinRange = 0
, kMaxRange = fromIntegral $ 2 ** systemBits / 2
}
rightKBucket = KBucket {
  kContent = V.fromList []
, kMinRange = fromIntegral $ 2 ** systemBits / 2
, kMaxRange = 2 ** systemBits
}

fullContent = V.generate systemK genF where
  genF i = (defaultPeer {nodeId = fromIntegral i}, LastSeen 0)

defaultPeer = Peer 0 $ SockAddrUnix ""
