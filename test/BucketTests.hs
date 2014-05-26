module BucketTests (kBucket, routingTable) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Vector ((!), (//))
import           Network.DHT.Kademlia.Bucket
import           Network.DHT.Kademlia.Def hiding (thisNode)
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
  describe "empty bucket behavior" $ do
    it "adds the first node to the only k-bucket" $
      addFirstNode `shouldReturn` True
  
  describe "node 7 adding" $ do
    it "[0,1,2,3,4,5,6]" $
      fillUpTable 7 [0,1,2,3,4,5,6] `shouldReturn` [[0,1],[4,5],[6]]
  
  describe "node 0 adding" $ do
    it "[5,4]" $
      fillUpTable 0 [5,4] `shouldReturn` [[5,4], [], []]
    it "[5,4,3]" $ 
      fillUpTable 0 [5,4,3] `shouldReturn` [[3], [5,4], []]
    it "[5,4,3,2]" $ 
      fillUpTable 0 [5,4,3,2] `shouldReturn` [[3,2], [5,4], []]
    it "[5,4,3,2,1,6]" $
      fillUpTable 0 [5,4,3,2,1,6] `shouldReturn` [[1], [3,2], [5,4]]
    it "[5,4,3,2,1,6,7]" $
      fillUpTable 0 [5,4,3,2,1,6,7] `shouldReturn` [[1], [3,2], [5,4]]
    it "[2,4]" $
      fillUpTable 0 [2,4] `shouldReturn` [[2,4], [], []]
    it "[2,4,5]" $
      fillUpTable 0 [2,4,5] `shouldReturn` [[2], [4,5], []]
    it "[2,4,5,3]" $
      fillUpTable 0 [2,4,5,3] `shouldReturn` [[2,3], [4,5], []]
    it "[2,4,5,3,1]" $
      fillUpTable 0 [2,4,5,3,1] `shouldReturn` [[1], [2,3], [4,5]]
  
  describe "full bucket behavior" $ do
    it "splits at index 0 into 2 buckets at indices 0 and 1" $
      (rtOneFullBucket >>= split) `shouldReturn` (leftKBucket, rightKBucket)
    it "splits when adding a node and the node's id is in this node's range" $
      (rtOneFullBucket >>= addNodeInRange newNode) `shouldReturn` True
    it "drops node when bucket is full and node's id is not in this node's range" $
      addNodeDrop `shouldReturn` True

fillUpTable :: Double -> [Double] -> IO [[Double]]
fillUpTable thisId otherIds = do
  rt <- atomically $ defaultRoutingTable Nothing
  forM_ otherIds $ \i -> addNode thisNode rt defaultNode {nodeId = i}
  rt2 <- stripSTM rt
  return $ map (map (nodeId . fst) . V.toList . kContent) $ V.toList rt2
  where
    thisNode = defaultNode {nodeId = thisId}

newNode :: Node
newNode = defaultNode {nodeId = 7} -- fullBucket range is 0-7

thisNode :: Node
thisNode = defaultNode {nodeId = 1}

-- the single bucket split and the newNode exists in it
splitBucket = rightKBucket {
  kContent = V.fromList [(newNode, LastSeen 0)]
}

addFirstNode :: IO Bool
addFirstNode = do
  rt <- atomically $ defaultRoutingTable Nothing
  e <- addNode thisNode rt newNode
  case e of
    Left _ -> return False
    Right _ -> do
      rt' <- stripSTM rt
      return $ newKBucketWithNode ~= (rt' ! 0)
  where
    newKBucketWithNode = KBucket {
      kContent = V.fromList [(newNode, LastSeen 0)]
    , kMinRange = 0
    , kMaxRange = 2 ** systemBits
    }

addNodeInRange :: Node -> RoutingTable -> IO Bool
addNodeInRange thatNode rt = do
  e <- addNode thisNode rt thatNode
  case e of
    Left _ -> return False
    Right _ -> do
      rt' <- stripSTM rt
      return $ leftKBucket ~= (rt' ! 0)
            && splitBucket ~= (rt' ! 1)

splitDropBucket = rightKBucket {
  kContent = V.fromList [
    (defaultNode {nodeId = 6}, LastSeen 0)
  , (defaultNode {nodeId = 7}, LastSeen 0)
  ]
}

addNodeDrop :: IO Bool
addNodeDrop = do
  rt <- rtOneFullBucket
  
  -- cause a split and fill second k-bucket
  addNode thisNode rt $ fst $ (kContent splitDropBucket ! 0)
  addNode thisNode rt $ fst $ (kContent splitDropBucket ! 1)
  -- second k-bucket is now full
  
  -- ignore new node 5 in second bucket [4,8)
  e <- addNode thisNode rt $ defaultNode {nodeId = 5}
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
  rt <- defaultRoutingTable Nothing
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
  genF i = (defaultNode {nodeId = fromIntegral i}, LastSeen 0)

defaultNode = Node 0 $ SockAddrUnix ""
