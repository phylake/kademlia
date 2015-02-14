{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.DHT.Kademlia.Workers.Interactive (interactive) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Aeson as JSON
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Network
import           Network.DHT.Kademlia.Def
import           Network.DHT.Kademlia.Util
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Vector as V

-- | Interactive shell to inspect Kademlia
--
-- Example usage
--
-- > echo routes | netcat localhost 6000
interactive :: KademliaEnv -> IO ()
interactive KademliaEnv{..} = forkIO_ $ do
  runTCPServer (serverSettings 6000 "!4") app
  where
    app req = do
      bs <- appSource req $$
            CB.takeWhile (/= newline) =$ CL.consume >>= return . BL.fromChunks
      case bs of
        "routes" -> do
          routingTable2 <- liftM (V.takeWhile (/= defaultKBucket)) $
                           atomically $ V.mapM readTVar routingTable
          CB.sourceLbs (JSON.encode routingTable2) $$ appSink req
        otherwise -> CB.sourceLbs usage $$ appSink req

newline :: Word8
newline = head $ B.unpack $ BC.singleton '\n'

usage :: BL.ByteString
usage = BL.intercalate "\n" [
    "USAGE"
  , "  routes - dump current routing table as json"
  , ""
  ]
