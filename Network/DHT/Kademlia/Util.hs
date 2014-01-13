module Network.DHT.Kademlia.Util where

import           Data.Time
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

epochNow :: IO Double
epochNow = getCurrentTime >>= utcToEpoch where
  utcToEpoch :: UTCTime -> IO Double
  utcToEpoch = return . fromIntegral . round . utcTimeToPOSIXSeconds

secToMicro :: Int -> Int
secToMicro t = t * fromIntegral (10 ** 6 :: Double)

instance Integral Float where
  quotRem a b = (fab, (ab - fab)*b)
    where
      ab = a/b
      fab = floor ab
  toInteger = floor

instance Integral Double where
  quotRem a b = (fab, (ab - fab)*b)
    where
      ab = a/b
      fab = floor ab
  toInteger = floor
