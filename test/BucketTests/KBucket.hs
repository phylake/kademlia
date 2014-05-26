module BucketTests.KBucket (kBucket) where

import           TestPrelude
import qualified Data.Vector as V

kBucket :: Spec
kBucket = describe "k-bucket" $ do
  it "splits range in [kMinRange, kMaxRange)" $
    splitKBucketImpl fullKBucket `shouldBe` (leftKBucket, rightKBucket)

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
