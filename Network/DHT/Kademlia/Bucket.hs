{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.DHT.Kademlia.Bucket where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Bits
import           Data.Vector ((!), (!?), (//))
import           Network.DHT.Kademlia.Def
import           Network.DHT.Kademlia.Util
import           Util.Time
import qualified Data.Vector as V

addNode :: Node -- ^ this node
        -> RoutingTable
        -> Node -- ^ other node
        -> IO (Either String ())
addNode this@(Node thisNodeId _) kbuckets that@(Node thatNodeId _)
  | this == that = return $ Right ()
  | otherwise = do
      now <- lastSeen
      e <- case kbuckets !? kBucketIdx of
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
      case e of
        Left err -> return $ Left err
        Right () -> return $ Right ()
  where
    kBucketIdx = floor $ logBase 2 $ fromIntegral thisXORThat
    thisXORThat = (toInteger thisNodeId) `xor` (toInteger thatNodeId)
