module Main where

import           Data.Vector ((!), (//))
import           Network.DHT.Kademlia.Def
import           Network.DHT.Kademlia.Util
import           Network.DHT.Kademlia.Bucket
import           Network.Socket
import           Test.Hspec
import           Util.Time
import qualified Data.Vector as V

main :: IO ()
main = hspec $ 
  describe "Kademlia" $ do
    testAssumptions
    --kBucket

testAssumptions = describe "test assumptions" $ do
    it "systemK is 2" $ systemK `shouldBe` 2
    it "systemBits is 3" $ systemBits `shouldBe` 3

kBucket = describe "k-bucket" $ do
  it "split" $
    splitKBucketImpl defaultKBucket `shouldBe` (defaultKBucket, defaultKBucket)
  where
    fullBucket = KBucket {
      kContent = V.fromList [(defaultPeer {nodeId = 1}, 0)]
    , kMinRange = 0
    , kMaxRange = 0
    }

defaultPeer = Peer 0 $ SockAddrUnix ""
