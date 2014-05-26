module BucketTests.KBucket (kBucket) where

import           TestPrelude
import qualified Data.Vector as V

kBucket :: Spec
kBucket = describe "k-bucket" $ do
  it "splits range in [kMinRange, kMaxRange)" $
    splitKBucketImpl fullKBucket `shouldBe` (leftKBucket, rightKBucket)
