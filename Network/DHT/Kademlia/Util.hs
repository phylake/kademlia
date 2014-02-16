{-# LANGUAGE RecordWildCards #-}
module Network.DHT.Kademlia.Util where

import           Control.Concurrent.STM
import           Data.Binary
import           Data.Word
import           GHC.IO.Handle
import           GHC.IO.Handle.FD
import           Network.DHT.Kademlia.Def
import           Util.Integral
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

pStdOut = hPutStr stdout
pStdErr = hPutStr stderr

dumpRT :: RoutingTable -> IO ()
dumpRT kbuckets = do
  V.mapM_ dumpBuckets $ V.zip (V.fromList [0..V.length kbuckets-1]) kbuckets
  where
    dumpBuckets (bit, tvb) = do
      putStrLn $ show bit ++ " -----------------------------"
      KBucket{..} <- atomically $ readTVar tvb
      mapM_ (putStrLn . show) $ V.toList kContent

secToMicro :: Int -> Int
secToMicro t = t * fromIntegral (10 ** 6 :: Double)

storeChunks :: B.ByteString -> B.ByteString -> [RPC]
storeChunks k v = loop 0 k v
  where
    totalChunks :: Word32
    totalChunks = fromIntegral
                $ ceiling
                $ (fromIntegral $ B.length v) / (fromIntegral chunkBytes)

    loop i k v
      | B.null v = []
      | otherwise = rpc : loop (i+1) k rest
      where
        rpc = RPC_STORE_REQ k i totalChunks chunkLen chunk
        (chunk, rest) = B.splitAt chunkBytes v
        chunkLen = fromIntegral $ B.length chunk

-- TODO validate checksum
tryReassemble :: V.Vector B.ByteString -> Maybe B.ByteString
tryReassemble v = if V.null v || V.any (== B.empty) v
  then Nothing
  else Just $ V.foldl1 B.append v
