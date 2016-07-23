{-# LANGUAGE RecordWildCards #-}
module TestPrelude (
  module Control.Concurrent.STM
, module Control.Monad
, module Data.Vector
, module Network.DHT.Kademlia.Bucket
, module Network.DHT.Kademlia.Def
, module Test.Hspec
, module TestEq

, addNodeSimple
, newEnv
, sendNoop
, defaultNode
, fullKBucket
, leftKBucket
, rightKBucket
) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Vector ((!), (//))
import           Network.DHT.Kademlia.Bucket
import           Network.DHT.Kademlia.Def hiding (thisNode)
import           Network.Socket hiding (send)
import           Test.Hspec
import           TestEq
import qualified Data.HashTable.IO as H
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

newEnv :: IO KademliaEnv
newEnv = do
  sock <- socket AF_INET Datagram defaultProtocol
  mvStoreHT <- H.new >>= newMVar
  dataStore <- defaultDataStore
  pingREQs <- atomically $ newTVar V.empty
  routingTable <- atomically $ defaultRoutingTable Nothing
  return KademliaEnv{..}
  where
    logDebug _ = return ()
    logInfo _ = return ()
    logWarn _ = return ()
    logError _ = return ()

sendNoop :: RPC -> IO ()
sendNoop _ = return ()

defaultNode = Node 0 $ SockAddrUnix ""

fullKBucket = KBucket {
  kContent = fullContent
}

leftKBucket = KBucket {
  kContent = fullContent
}

rightKBucket = KBucket {
  kContent = V.fromList []
}

fullContent = V.generate systemK genF where
  genF i = (defaultNode {nodeId = fromIntegral i}, LastSeen 0)
