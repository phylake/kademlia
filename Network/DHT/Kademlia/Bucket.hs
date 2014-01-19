{-# LANGUAGE RecordWildCards #-}
module Network.DHT.Kademlia.Bucket (addPeer, findKBucket, splitKBucket) where

import           Control.Concurrent.STM
import           Data.Vector ((!), (//))
import           Network.DHT.Kademlia.Def
import           Network.DHT.Kademlia.Util
import           Util.Time
import qualified Data.Vector as V

-- | Psuedo code for 2.4
-- given this node's id ID and a new peer C
--
-- > function ADD_PEER()
-- >   get k-bucket B for C
-- >   if B.length < k then add C to tail
-- >   else if C is in B then move C to tail
-- >   else
-- >     if B.minRange <= ID && ID < B.maxRange then SPLIT() and ADD_PEER()
-- >     else ignore C
addPeer :: Peer -- ^ this node
        -> RoutingTable
        -> Peer -- ^ other node
        -> IO (Either String ())
addPeer = addPeerLoop 0

addPeerLoop :: Int -- ^ number of times this function has called itself
            -> Peer -- ^ this node
            -> RoutingTable
            -> Peer -- ^ other node
            -> IO (Either String ())
addPeerLoop xcalled this rt that
  | xcalled > 1 = return $ Left "infinite loop would occur"
  | this == that = return $ Right ()
  | otherwise = do
      now <- epochNow
      e <- atomically $ do
        kbuckets <- readTVar rt -- (V.Vector (TVar KBucket))
        mKB <- findKBucket that kbuckets
        case mKB of
          Nothing -> return $ Left $ "no k-bucket found for [" ++ show that ++ "]"
          Just (kb@KBucket{..}, routeIdx) ->
            case V.findIndex ((==) that . fst) kContent of
              Just peerIdx -> do
                -- TODO sort here or before use
                let kb' = kb {kContent = kContent // [(peerIdx, (that, now))]}
                writeTVar (kbuckets ! routeIdx) kb'
                return $ Right False
              Nothing -> if V.length kContent < systemK
                then do
                  let kb' = kb {kContent = V.snoc kContent (that, now)}
                  writeTVar (kbuckets ! routeIdx) kb'
                  return $ Right False
                else do
                  splitKBucket routeIdx rt
                  return $ Right True
      case e of
        Left err -> return $ Left err
        Right False -> return $ Right ()
        Right True -> addPeerLoop (xcalled+1) this rt that

findKBucket :: Peer -- ^ the peer to find
            -> V.Vector (TVar KBucket)
            -> STM (Maybe (KBucket, Int)) -- ^ Maybe (found bucket, index)
findKBucket (Peer key _) vec = loop (V.length vec - 1) where
  loop idx
    | idx < 0 = return Nothing
    | otherwise = do
        kb@(KBucket{..}) <- readTVar $ vec ! idx
        if kMinRange <= key && key < kMaxRange
          then return $ Just (kb, idx)
          else loop (idx - 1)

splitKBucket :: Int
             -> RoutingTable
             -> STM ()
splitKBucket rt idx = undefined
