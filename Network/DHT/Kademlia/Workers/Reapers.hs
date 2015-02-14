{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
#ifdef TEST
module Network.DHT.Kademlia.Workers.Reapers (pingREQReaper, pingREQReaperImpl) where
#else
module Network.DHT.Kademlia.Workers.Reapers (pingREQReaper) where
#endif

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Time.Clock
import           Network.DHT.Kademlia.Bucket
import           Network.DHT.Kademlia.Def
import           Network.DHT.Kademlia.Util
import qualified Data.Vector as V

-- | Outstanding outbound PING requests need to be cleaned up.
-- 
-- Partition the vector into expired and unexpired ping requests.
-- 
-- Discard the expired requests and update k-buckets accordingly.
pingREQReaper :: KademliaEnv -> IO ()
pingREQReaper env = forkIO_ $ forever $ do
  threadDelay $ secToMicro 2 -- should be >= threshold
  now <- getCurrentTime
  pingREQReaperImpl env now threshold
  where
    threshold = 1 -- should be <= threadDelay

pingREQReaperImpl :: KademliaEnv -> UTCTime -> NominalDiffTime -> IO ()
pingREQReaperImpl KademliaEnv{..} now threshold = atomically $ do
  pings <- readTVar pingREQs
  
  let (expired, rest) = V.partition (\(t,_) -> diffUTCTime now t > threshold) pings
  V.mapM (fBucket $ V.map snd expired) routingTable
  
  writeTVar pingREQs rest
  where
    fBucket :: V.Vector Node -> TVar KBucket -> STM ()
    fBucket expired tv = do
      kb@KBucket{..} <- readTVar tv
      writeTVar tv $ kb {kContent = V.filter (fContent expired) kContent}
    
    -- keep unexpired nodes
    fContent :: V.Vector Node -> (Node, LastSeen) -> Bool
    fContent expired (p,_) = not $ V.elem p expired
