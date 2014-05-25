module TestEq (TestEq(..)) where

import           Network.DHT.Kademlia.Def
import qualified Data.Vector as V

-- | For the purposes of testing I need to relax equality for things
-- like LastSeen which I can't test and SockAddr which I don't care about
class TestEq a where
  (~=) :: a -> a -> Bool

instance TestEq Node where
  (Node a _) ~= (Node b _) = a == b -- ignore SockAddr

instance TestEq KBucket where
  (KBucket ac amin amax) ~= (KBucket bc bmin bmax) =
       amin == bmin
    && amax == bmax
    -- ignore LastSeen
    && V.map fst ac ~= V.map fst bc

instance (TestEq a) => TestEq (V.Vector a) where
  a ~= b = V.length a == V.length b
        && (V.all (\(a, b) -> a ~= b) $ V.zip a b)
