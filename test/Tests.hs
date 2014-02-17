{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           BucketTests
import           SerializationTests

import           Control.Concurrent.STM
import           Control.Monad (liftM2)
import           Network.DHT.Kademlia.Def
import           Test.Hspec
import qualified Data.Vector as V

main :: IO ()
main = hspec $ 
  describe "Kademlia" $ do
    systemAssumptions
    tVarAssumptions
    kBucket
    routingTable
    serialization

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

rpcInterface :: RPCHooks -> Spec
rpcInterface = undefined
{-rpcInterface = describe "RPC interface" $ do
  it "splits range in [kMinRange, kMaxRange)" $
    splitKBucketImpl fullKBucket `shouldBe` (leftKBucket, rightKBucket)-}
