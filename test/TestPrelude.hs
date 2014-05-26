module TestPrelude (
  module Control.Concurrent.STM
, module Control.Monad
, module Data.Vector
, module Network.DHT.Kademlia.Bucket
, module Network.DHT.Kademlia.Def
, module Network.Socket
, module Test.Hspec
, module TestEq
, module Data.Vector

, addNodeSimple
, stripSTM
, defaultNode
) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Vector ((!), (//))
import           Network.DHT.Kademlia.Bucket
import           Network.DHT.Kademlia.Def hiding (thisNode)
import           Network.Socket (SockAddr(SockAddrUnix))
import           Test.Hspec
import           TestEq
import qualified Data.Vector as V

-- | Node ids in, node ids out
addNodeSimple :: Double -> [Double] -> IO [[Double]]
addNodeSimple thisId otherIds = do
  rt <- atomically $ defaultRoutingTable Nothing
  forM_ otherIds $ \i -> addNode thisNode rt defaultNode {nodeId = i}
  rt2 <- stripSTM rt
  return $ map (map (nodeId . fst) . V.toList . kContent) $ V.toList rt2
  where
    thisNode = defaultNode {nodeId = thisId}

stripSTM :: RoutingTable -> IO (V.Vector KBucket)
stripSTM = V.mapM (atomically . readTVar)

defaultNode = Node 0 $ SockAddrUnix ""
