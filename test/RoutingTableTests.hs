module RoutingTableTests (routingTableSpec) where

import           TestPrelude
import qualified Data.Vector as V

routingTableSpec :: Spec
routingTableSpec = describe "routing table" $ do
  describe "empty bucket behavior" $ do
    it "adds the first node to the only k-bucket" $
      addFirstNode `shouldReturn` True
  
  describe "node 0 adding" $ do
    it "[5,4]           yields [[], [], [5,4]]" $
      addNodeSimple 0 [5,4] `shouldReturn` [[], [], [5,4]]
    it "[5,4,3]         yields [[], [3], [5,4]]" $
      addNodeSimple 0 [5,4,3] `shouldReturn` [[], [3], [5,4]]
    it "[5,4,3,2]       yields [[], [3,2], [5,4]]" $
      addNodeSimple 0 [5,4,3,2] `shouldReturn` [[], [3,2], [5,4]]
    it "[5,4,3,2,1,6]   yields [[1], [3,2], [5,4]]" $
      addNodeSimple 0 [5,4,3,2,1,6] `shouldReturn` [[1], [3,2], [5,4]]
    it "[5,4,3,2,1,6,7] yields [[1], [3,2], [5,4]]" $
      addNodeSimple 0 [5,4,3,2,1,6,7] `shouldReturn` [[1], [3,2], [5,4]]
    it "[2,4]           yields [[], [2], [4]]" $
      addNodeSimple 0 [2,4] `shouldReturn` [[], [2], [4]]
    it "[2,4,5]         yields [[], [2], [4,5]]" $
      addNodeSimple 0 [2,4,5] `shouldReturn` [[], [2], [4,5]]
    it "[2,4,5,3]       yields [[], [2,3], [4,5]]" $
      addNodeSimple 0 [2,4,5,3] `shouldReturn` [[], [2,3], [4,5]]
    it "[2,4,5,3,1]     yields [[1], [2,3], [4,5]]" $
      addNodeSimple 0 [2,4,5,3,1] `shouldReturn` [[1], [2,3], [4,5]]
  
  describe "node 7" $ do
    it "adding [0,1,2,3,4,5,6] yields [[6],[4,5],[0,1]]" $
      addNodeSimple 7 [0..6] `shouldReturn` [[6],[4,5],[0,1]]
    it "and its k closest nodes are [6,5]" $
      getKClosestNodes `shouldReturn` [6,5]

node1 :: Node
node1 = defaultNode {nodeId = 1}

node7 :: Node
node7 = defaultNode {nodeId = 7} -- fullBucket range is 0-7

addFirstNode :: IO Bool
addFirstNode = do
  rt <- atomically $ defaultRoutingTable Nothing
  e <- addNode node1 rt node7
  case e of
    Left _ -> return False
    Right _ -> do
      rt' <- stripSTM rt
      return $ newKBucketWithNode ~= (rt' ! 2)
  where
    newKBucketWithNode = KBucket {
      kContent = V.fromList [(node7, LastSeen 0)]
    }

getKClosestNodes :: IO [NodeId]
getKClosestNodes = do
  rt <- atomically $ defaultRoutingTable Nothing
  forM_ [0..6] $ \i -> addNode node7 rt defaultNode {nodeId = i}
  closest <- kClosestNodes (nodeId node7) rt
  return $ V.toList $ V.map nodeId closest
