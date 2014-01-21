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

main :: IO ()
main = hspec $ 
  describe "Kademlia" $ do
    systemAssumptions
    tVarAssumptions
    kBucket
    routingTable

tVarAssumptions :: Spec
tVarAssumptions = describe "TVar" $ do
  it "2 separate TVars with equal contents are not equal" $
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
  it "splits full bucket at index 0 into 2 buckets at indices 0 and 1" $
    (doSplit >>= getSplitResult) `shouldReturn` (leftKBucket, rightKBucket)
  where
    doSplit :: IO RoutingTable
    doSplit = atomically $ do
      rt <- defaultRoutingTable
      writeTVar (rt ! 0) fullKBucket
      splitKBucket 0 rt
      return rt

    getSplitResult :: RoutingTable -> IO (KBucket, KBucket)
    getSplitResult rt = atomically $
      liftM2 (,)
        (readTVar $ rt ! 0)
        (readTVar $ rt ! 1)

fullKBucket = KBucket {
  kContent = V.fromList [
      (defaultPeer {nodeId = 0}, 0)
    , (defaultPeer {nodeId = 1}, 0)
    , (defaultPeer {nodeId = 2}, 0)
    , (defaultPeer {nodeId = 3}, 0)
    , (defaultPeer {nodeId = 4}, 0)
    , (defaultPeer {nodeId = 5}, 0)
    , (defaultPeer {nodeId = 6}, 0)
    , (defaultPeer {nodeId = 7}, 0)
    ]
, kMinRange = 0
, kMaxRange = 8
}
leftKBucket = KBucket {
  kContent = V.fromList [
      (defaultPeer {nodeId = 0}, 0)
    , (defaultPeer {nodeId = 1}, 0)
    , (defaultPeer {nodeId = 2}, 0)
    , (defaultPeer {nodeId = 3}, 0)
    ]
, kMinRange = 0
, kMaxRange = 4
}
rightKBucket = KBucket {
  kContent = V.fromList [
      (defaultPeer {nodeId = 4}, 0)
    , (defaultPeer {nodeId = 5}, 0)
    , (defaultPeer {nodeId = 6}, 0)
    , (defaultPeer {nodeId = 7}, 0)
    ]
, kMinRange = 4
, kMaxRange = 8
}

defaultPeer = Peer 0 $ SockAddrUnix ""
