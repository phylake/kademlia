{-# LANGUAGE RecordWildCards #-}
module Network.DHT.Kademlia.Bucket (
  nodeDist
, kBucketIndex
, addNode
, kClosestNodes
, stripSTM
) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Bits
import           Data.List
import           Data.Vector ((!), (!?), (//))
import           Network.DHT.Kademlia.Def
import           Network.DHT.Kademlia.Util
import           Util.Time
import qualified Data.Vector as V

nodeDist :: NodeId -> NodeId -> NodeId
nodeDist n1 n2 = fromIntegral $ (toInteger n1) `xor` (toInteger n2)

kBucketIndex :: NodeId -> NodeId -> Int
kBucketIndex n1 n2 = floor $ logBase 2 $ nodeDist n1 n2

addNode :: Node -- ^ this node
        -> RoutingTable
        -> Node -- ^ other node
        -> IO (Either String ())
addNode this@(Node thisNodeId _) kbuckets that@(Node thatNodeId _)
  | this == that = return $ Right ()
  | otherwise = do
      now <- lastSeen
      case kbuckets !? kBucketIdx of
        Nothing -> return $ Left $ "no k-bucket found for [" ++ show that ++ "]"
        Just tVar -> atomically $ do
          kb@KBucket{..} <- readTVar tVar
          case V.findIndex ((==) that . fst) kContent of
            -- we already know this node. update last seen time
            Just thatIdx -> do
              -- TODO sort here or before use
              let kb' = kb {kContent = kContent // [(thatIdx, (that, now))]}
              writeTVar (kbuckets ! kBucketIdx) kb'
              return $ Right ()
            -- we didn't know this node. add it to our kbucket if it has room
            Nothing | V.length kContent < systemK -> do
                let kb' = kb {kContent = V.snoc kContent (that, now)}
                writeTVar (kbuckets ! kBucketIdx) kb'
                return $ Right ()
            Nothing -> return $ Right ()
  where
    kBucketIdx = kBucketIndex thisNodeId thatNodeId

-- | The 'systemK' closest nodes to this node
{- TODO optimize by sorting closest k buckets so i can exit early -}
kClosestNodes :: NodeId -> RoutingTable -> IO (V.Vector Node)
kClosestNodes thisNodeId rt = stripSTM rt >>= return . systemKClosest
  where
    systemKClosest :: V.Vector KBucket -> V.Vector Node
    systemKClosest = V.fromList                 -- V.Vector Node
                   . take systemK               -- [Node]
                   . sortBy sortF               -- [Node]
                   . map fst                    -- [Node]
                   . foldr (++) []              -- [(Node, LastSeen)]
                   . map (V.toList . kContent)  -- [[(Node, LastSeen)]]
                   . V.toList                   -- [KBucket]
                                                -- V.Vector KBucket

    sortF :: Node -> Node -> Ordering
    sortF n1 n2 = compare (nodeDist thisNodeId $ nodeId n1)
                          (nodeDist thisNodeId $ nodeId n2)

{-# INLINE stripSTM #-}
stripSTM :: RoutingTable -> IO (V.Vector KBucket)
stripSTM = V.mapM readTVarIO
