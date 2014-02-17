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
import           Data.Vector ((!), (//))
import           Network.DHT.Kademlia.Bucket
import           Network.DHT.Kademlia.Def
import           Network.DHT.Kademlia.Util
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashTable.IO as H
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Outstanding PING requests that timeout need to update k-buckets accordingly
pingREQReaper :: Int -> KademliaEnv -> IO ()
pingREQReaper timeout env@(KademliaEnv{..}) = void $ forkIO loop where
  loop = do
    threadDelay timeout
    loop
