{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
--module Network.DHT.Kademlia.Bucket (addPeer, findKBucket, splitKBucket) where
module Network.DHT.Kademlia.Bucket where

import           Control.Concurrent.STM
import           Data.Vector ((!), (//))
import           Network.DHT.Kademlia.Def
import           Network.DHT.Kademlia.Util
import           Util.Time
import qualified Data.Vector as V

-- | Ensure [kMinRange, kMaxRange)
idInRange :: (Ord a) => a -> a -> a -> Bool
idInRange id min max = min <= id && id < max

-- | Psuedo code for 2.4
-- given this node's id ID and a new peer C
--
-- > function ADD_PEER()
-- >   get k-bucket B for C
-- >   if C is in B then move C to tail
-- >   else if B.length < k then add C to tail
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
addPeerLoop xcalled this kbuckets that
  | xcalled > 1 = return $ Left "infinite loop would occur"
  | this == that = return $ Right ()
  | otherwise = do
      now <- epochNow
      e <- atomically $ do
        mKB <- findKBucket that kbuckets
        case mKB of
          Nothing -> return $ Left $ "no k-bucket found for [" ++ show that ++ "]"
          Just (kb@KBucket{..}, routeIdx) ->
            case V.findIndex ((==) that . fst) kContent of
              Just thatIdx -> do
                -- TODO sort here or before use
                let kb' = kb {kContent = kContent // [(thatIdx, (that, now))]}
                writeTVar (kbuckets ! routeIdx) kb'
                return $ Right False
              Nothing -> if V.length kContent < systemK
                then do
                  let kb' = kb {kContent = V.snoc kContent (that, now)}
                  writeTVar (kbuckets ! routeIdx) kb'
                  return $ Right False
                else do
                  splitKBucket routeIdx kbuckets
                  return $ Right True
      case e of
        Left err -> return $ Left err
        Right False -> return $ Right ()
        Right True -> addPeerLoop (xcalled+1) this kbuckets that

findKBucket :: Peer -- ^ the peer to find
            -> V.Vector (TVar KBucket)
            -> STM (Maybe (KBucket, Int)) -- ^ Maybe (found bucket, index)
findKBucket (Peer key _) vec = loop (V.length vec - 1) where
  loop idx
    | idx < 0 = return Nothing
    | otherwise = do
        kb@(KBucket{..}) <- readTVar $ vec ! idx
        if idInRange key kMinRange kMaxRange
          then return $ Just (kb, idx)
          else loop (idx - 1)

splitKBucket :: Int -- ^ Index of bucket to split
             -> RoutingTable
             -> STM ()
splitKBucket idx kbuckets = do
  kb <- readTVar (kbuckets ! idx)
  let (kbl, kbr) = splitKBucketImpl kb
  writeTVar (kbuckets !  idx     ) kbl
  writeTVar (kbuckets ! (idx + 1)) kbr
  return ()

splitKBucketImpl :: KBucket -> (KBucket, KBucket)
splitKBucketImpl KBucket{..} = (lBucket, rBucket)
  where
    kMidRange = floor $ kMaxRange - (kMaxRange - kMinRange)/2
    
    lBucket = defaultKBucket {
      kContent = lContent
    , kMinRange = kMinRange
    , kMaxRange = kMidRange
    }
    rBucket = defaultKBucket {
      kContent = rContent
    , kMinRange = kMidRange
    , kMaxRange = kMaxRange
    }
    
    (lContent, rContent) = V.foldl splitFold (V.empty, V.empty) kContent

    splitFold :: (V.Vector (Peer, LastSeen), V.Vector (Peer, LastSeen))
              -> (Peer, LastSeen)
              -> (V.Vector (Peer, LastSeen), V.Vector (Peer, LastSeen))
    splitFold (l,r) t@(Peer nodeId _, _) =
      if idInRange nodeId kMidRange kMaxRange
        then (l, V.snoc r t) -- snoc on left fold
        else (V.snoc l t, r)
