{-# LANGUAGE RecordWildCards #-}
module Network.DHT.Kademlia.Bucket (
  nodeDist
, kBucketIndex
, addNode
, kClosestNodes
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
            Nothing -> if V.length kContent < systemK
              then do
                let kb' = kb {kContent = V.snoc kContent (that, now)}
                writeTVar (kbuckets ! kBucketIdx) kb'
                return $ Right ()
              else return $ Right () 
  where
    kBucketIdx = kBucketIndex thisNodeId thatNodeId

-- | The 'systemK' closest and most recently seen nodes to this node
kClosestNodes :: NodeId -> RoutingTable -> IO (V.Vector Node)
kClosestNodes thisNodeId = V.foldM foldF V.empty
  where
    foldF :: V.Vector Node -> TVar KBucket -> IO (V.Vector Node)
    foldF acc tVar
      | V.length acc == systemK = return acc
      | otherwise = atomically $ do
          kb <- readTVar tVar
          return $ acc V.++ nClosestNodes (systemK - V.length acc) kb

    nClosestNodes :: Int -> KBucket -> V.Vector Node
    nClosestNodes n = V.fromList
                    . take n
                    . sortBy sortF
                    . map fst
                    . V.toList
                    . kContent

    sortF :: Node -> Node -> Ordering
    sortF n1 n2 = compare (nodeDist thisNodeId $ nodeId n1)
                          (nodeDist thisNodeId $ nodeId n2)
