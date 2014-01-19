module Network.DHT.Kademlia.Util where

import           GHC.IO.Handle
import           GHC.IO.Handle.FD
import           Util.Integral

pStdOut = hPutStr stdout
pStdErr = hPutStr stderr

secToMicro :: Int -> Int
secToMicro t = t * fromIntegral (10 ** 6 :: Double)
