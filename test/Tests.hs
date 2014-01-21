{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Vector ((!), (//))
import           Network.DHT.Kademlia.Bucket
import           Network.DHT.Kademlia.Def
import           Network.DHT.Kademlia.Util
import           Network.Socket
import           Test.Hspec
import           Util.Time
import qualified Data.Vector as V

-- | For the purposes of testing I need to relax equality for things
-- like LastSeen which I can't test and SockAddr which I don't care about
class TestEq a where
  (~=) :: a -> a -> Bool

instance TestEq Peer where
  (Peer a _) ~= (Peer b _) = a == b -- ignore SockAddr

instance TestEq KBucket where
  (KBucket ac amin amax) ~= (KBucket bc bmin bmax) =
       amin == bmin
    && amax == bmax
    -- ignore LastSeen
    && V.map fst ac ~= V.map fst bc

instance (TestEq a) => TestEq (V.Vector a) where
  a ~= b = V.length a == V.length b
        && (V.all (\(a, b) -> a ~= b) $ V.zip a b)

main :: IO ()
main = hspec $ 
  describe "Kademlia" $ do
    systemAssumptions
    tVarAssumptions
    kBucket
    routingTable

tVarAssumptions :: Spec
tVarAssumptions = describe "TVar" $ do
  it "2 new TVars with equal contents are not equal" $
    twoTVarWithEqualContents `shouldReturn` True
  where
    twoTVarWithEqualContents :: IO Bool
    twoTVarWithEqualContents = do
      (tv1, tv2) <- atomically $ liftM2 (,) (newTVar 1) (newTVar 1)
      return $ tv1 /= tv2

systemAssumptions :: Spec
systemAssumptions = describe "system assumptions" $ do
  it "systemK is 2" $ systemK `shouldBe` 2
  it "systemBits is 3" $ systemBits `shouldBe` 3

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
  where
    newPeer :: Peer
    newPeer = defaultPeer {nodeId = 7} -- fullBucket range is 0-7
    
    thisNode :: Peer
    thisNode = defaultPeer {nodeId = 1}
    
    -- the single bucket split and the newPeer exists in it
    splitBucket = rightKBucket {
      kContent = V.fromList [(newPeer, 0)]
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
        (defaultPeer {nodeId = 6}, 0)
      , (defaultPeer {nodeId = 7}, 0)
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
  genF i = (defaultPeer {nodeId = fromIntegral i}, 0)

defaultPeer = Peer 0 $ SockAddrUnix ""
