{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.DHT.Kademlia.Workers.Reapers (pingREQReaper) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Timer
import           Control.Monad
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Network
import           Data.Time.Clock
import           Data.Vector ((!), (//))
import           Network.DHT.Kademlia.Bucket
import           Network.DHT.Kademlia.Def
import           Network.DHT.Kademlia.Util
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.HashTable.IO as H
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Outstanding PING requests that timeout need to update k-buckets accordingly
pingREQReaper :: KademliaEnv -> IO ()
pingREQReaper KademliaEnv{..} = forkIO_ $ forever $ do
  threadDelay $ secToMicro 2 -- should be >= threshold
  now <- getCurrentTime
  atomically $ do
    pings <- readTVar pingREQs
    
    let (expired, rest) = V.partition (\(t,_) -> diffUTCTime now t > threshold) pings
    V.mapM (fBucket $ V.map snd expired) rt
    
    writeTVar pingREQs rest
  where
    threshold = 1 -- should be <= threadDelay
    
    fBucket :: V.Vector Peer -> TVar KBucket -> STM ()
    fBucket expired tv = do
      kb@KBucket{..} <- readTVar tv
      writeTVar tv $ kb {kContent = V.filter (fContent expired) kContent}
    
    -- keep unexpired nodes
    fContent :: V.Vector Peer -> (Peer, LastSeen) -> Bool
    fContent expired (p,_) = not $ V.elem p expired
