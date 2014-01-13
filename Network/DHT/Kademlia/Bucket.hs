{-# LANGUAGE RecordWildCards #-}
module Network.DHT.Kademlia.Bucket where

import           Control.Concurrent.STM
import           Data.Vector ((!), (//))
import           Network.DHT.Kademlia.Def
import           Network.DHT.Kademlia.Util
import qualified Data.Vector as V

addPeer :: Peer -- ^ this node
        -> RoutingTable
        -> Peer -- ^ other node
        -> IO Bool
addPeer this rt that@(Peer thatNodeId _)
  | this == that = return False
  | otherwise = atomically $ do
      vbits <- readTVar rt -- (V.Vector (TVar KBucket))
      mKB <- findKBucket thatNodeId vbits
      case mKB of
        Nothing -> return False -- create k-bucket with split?
        Just (kb@KBucket{..}, routeIdx) ->
          case V.findIndex ((==) that . fst) kContent of
            Just peerIdx -> do
              let kb2 = kb {kContent = kContent // [(peerIdx, (that, 0))]} -- TODO epochNow
              writeTVar (vbits ! routeIdx) kb2
              return True
            Nothing -> if V.length kContent < systemK
              then do
                return False
              else do
                return True

-- Not sure if Key is less appropriate than Peer. Am I routing non-peer keys?
findKBucket :: Key -- ^ the key to find
            -> V.Vector (TVar KBucket)
            -> STM (Maybe (KBucket, Int)) -- ^ Maybe (found bucket, index)
findKBucket key vec = loop (V.length vec - 1) where
  loop idx
    | idx < 0 = return Nothing
    | otherwise = do
        kb@(KBucket{..}) <- readTVar $ vec ! idx
        if kMinRange < key && key < kMaxRange
          then return $ Just (kb, idx)
          else loop (idx - 1)
