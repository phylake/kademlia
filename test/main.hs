{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           RoutingTableTests
import           RPCTests
import           SerializationTests
import           TestPrelude

main :: IO ()
main = hspec $ 
  describe "Kademlia" $ do
    systemAssumptionsSpec
    tVarAssumptionsSpec
    routingTableSpec
    serializationSpec
    rpcSpec

tVarAssumptionsSpec :: Spec
tVarAssumptionsSpec = describe "TVar" $ do
  it "2 new TVars with equal contents are not equal" $
    twoTVarWithEqualContents `shouldReturn` True
  where
    twoTVarWithEqualContents :: IO Bool
    twoTVarWithEqualContents = do
      (tv1, tv2) <- atomically $ liftM2 (,) (newTVar 1) (newTVar 1)
      return $ tv1 /= tv2

systemAssumptionsSpec :: Spec
systemAssumptionsSpec = describe "system assumptions" $ do
  it "systemK is 2" $ systemK `shouldBe` 2
  it "systemBits is 3" $ systemBits `shouldBe` 3

--rpcInterface :: RPCHooks -> Spec
rpcInterface = undefined
{-rpcInterface = describe "RPC interface" $ do
  it "splits range in [kMinRange, kMaxRange)" $
    splitKBucketImpl fullKBucket `shouldBe` (leftKBucket, rightKBucket)-}
