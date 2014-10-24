{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.DHT.Kademlia.Bucket where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Vector ((!), (//))
import           Network.DHT.Kademlia.Def
import           Network.DHT.Kademlia.Util
import           Util.Time
import qualified Data.Vector as V

-- | Ensure [kMinRange, kMaxRange)
idInRange :: (Ord a) => a -> a -> a -> Bool
idInRange id min max = min <= id && id < max

-- | Psuedo code for 2.4
-- given this node's id ID and a new node C
--
-- > function ADD_PEER()
-- >   get k-bucket B for C
-- >   if C is in B then move C to tail
-- >   else if B.length < k then add C to tail
-- >   else
-- >     if B.minRange <= ID && ID < B.maxRange then SPLIT() and ADD_PEER()
-- >     else ignore C
addNode :: Node -- ^ this node
        -> RoutingTable
        -> Node -- ^ other node
        -> IO (Either String ())
addNode = addNodeLoop 0

addNodeLoop :: Int -- ^ number of times this function has called itself
            -> Node -- ^ this node
            -> RoutingTable
            -> Node -- ^ other node
            -> IO (Either String ())
addNodeLoop xcalled this@(Node thisNodeId _) kbuckets that
  | xcalled > 1 = return $ Left "infinite loop would occur"
  | this == that = return $ Right ()
  | otherwise = do
      now <- lastSeen
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
                else if idInRange thisNodeId kMinRange kMaxRange
                  then do
                    splitKBucket routeIdx kbuckets
                    return $ Right True
                  else return $ Right False
      case e of
        Left err -> return $ Left err
        Right False -> return $ Right ()
        Right True -> addNodeLoop (xcalled+1) this kbuckets that

findKBucket :: Node -- ^ the node to find
            -> V.Vector (TVar KBucket)
            -> STM (Maybe (KBucket, Int)) -- ^ Maybe (found bucket, index)
findKBucket (Node key _) vec = loop 0 where
  loop idx
    | idx >= V.length vec = return Nothing
    | otherwise = do
        kb@KBucket{..} <- readTVar $ vec ! idx
        if idInRange key kMinRange kMaxRange
          then return $ Just (kb, idx)
          else loop (idx + 1)

splitKBucket :: Int -- ^ Index of bucket to split
             -> RoutingTable
             -> STM ()
splitKBucket idx kbuckets = do
  readTVar nextTKb >>= shiftBuckets (V.drop (idx+1) kbuckets)
  kb <- readTVar (kbuckets ! idx)
  let (kbl, kbr) = splitKBucketImpl kb
  writeTVar currTKb kbl
  writeTVar nextTKb kbr
  return ()
  where
    currTKb = kbuckets !  idx
    nextTKb = kbuckets ! (idx + 1)

-- | If splitKBucket's idx is ever less than the greatest index of a non-default
-- bucket then information is lost. The set of non-default buckets beginning
-- at index idx need to be shifted to the right by 1
shiftBuckets :: V.Vector (TVar KBucket) -> KBucket -> STM ()
shiftBuckets kbuckets nextkb = V.foldM f nextkb kbuckets >> return ()
  where
    f :: KBucket -> TVar KBucket -> STM KBucket
    f kb tvKb = if kb == defaultKBucket
      -- don't care about defaultKBuckets so avoid transactions on the TVar
      then return kb
      else do
        kb2 <- readTVar tvKb
        writeTVar tvKb kb
        return kb2

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

    splitFold :: (V.Vector (Node, LastSeen), V.Vector (Node, LastSeen))
              -> (Node, LastSeen)
              -> (V.Vector (Node, LastSeen), V.Vector (Node, LastSeen))
    splitFold (l,r) t@(Node nodeId _, _) =
      if idInRange nodeId kMidRange kMaxRange
        then (l, V.snoc r t) -- snoc on left fold
        else (V.snoc l t, r)
