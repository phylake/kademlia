{-# LANGUAGE RecordWildCards #-}
module Network.DHT.Kademlia.Util where

import           Control.Concurrent.STM
import           GHC.IO.Handle
import           GHC.IO.Handle.FD
import           Network.DHT.Kademlia.Def
import           Util.Integral
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
