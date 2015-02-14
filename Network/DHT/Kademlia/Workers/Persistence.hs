{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.DHT.Kademlia.Workers.Persistence (persistRoutingTable) where

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

-- | Periodically persist the routing table incase this node goes offline
persistRoutingTable :: KademliaEnv -> Config -> IO ()
persistRoutingTable KademliaEnv{..} config = forkIO_ $ forever $ do
  threadDelay $ secToMicro 10
  writeRoutingTable fp routingTable
  where
    fp :: FilePath
    fp = T.unpack $ cfgRoutingTablePath config
