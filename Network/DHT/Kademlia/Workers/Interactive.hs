{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.DHT.Kademlia.Workers.Interactive (interactive) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Timer
import           Control.Monad
import           Data.Aeson as JSON
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Network
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

interactive :: KademliaEnv -> IO ()
interactive (KademliaEnv{..}) = do
  runTCPServer (serverSettings 6000 HostIPv4) app
  where
    app req = do
      bs <- appSource req $$
            CB.takeWhile (/= newline) =$ CL.consume >>= return . BL.fromChunks
      case bs of
        "routes" -> do
          rt2 <- atomically $ V.mapM readTVar rt
          CB.sourceLbs (JSON.encode rt2) $$ appSink req
        otherwise -> CB.sourceLbs usage $$ appSink req

newline :: Word8
newline = head $ B.unpack $ BC.singleton '\n'

usage :: BL.ByteString
usage = BL.intercalate "\n" [
    "USAGE"
  , "  routes - dump current routing table as json"
  , ""
  ]
