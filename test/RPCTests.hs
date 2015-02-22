{-# LANGUAGE RecordWildCards #-}
module RPCTests (rpcSpec) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.Time.Clock
import           Network.DHT.Kademlia
import           Network.DHT.Kademlia.Util (forkIO_)
import           Network.DHT.Kademlia.Workers.Reapers
import           Network.Socket (SockAddr(SockAddrInet))
import           System.Timeout
import           TestPrelude
import qualified Data.ByteString as B
import qualified Data.Vector as V

thisNode :: Node
thisNode = Node 0 (SockAddrInet 3030 2130706433)

thatNode :: Node
thatNode = Node 1 (SockAddrInet 4040 2130706433)

rpcSpec :: Spec
rpcSpec = describe "RPCs" $ do
  describe "RPC_PING" $ do
    it "receives pong" $ do
      pingPong `shouldReturn` Just True
    it "is tracked in pingREQs" $ do
      pingsAreTracked `shouldReturn` True
    it "and RPC_PING_REQS are cleaned up periodically" $ do
      pingsAreRemoved `shouldReturn` True
  describe "RPC_STORE" $ do
    it "transmits and reassembles chunked data" $ do
      storeChunkedData `shouldReturn` True

pingPong :: IO (Maybe Bool)
pingPong = do
  mv <- newEmptyMVar
  env <- newEnv
  let send = (\rpc -> do
                        case rpc of
                           RPC_PING_RES _ -> putMVar mv True
                           otherwise -> putMVar mv False
                        return ())
  forkIO_ $ receiveRPC env send (RPC_PING_REQ thatNode)
  timeout 1000 $ readMVar mv

pingsAreTracked :: IO Bool
pingsAreTracked = do
  env <- newEnv
  receiveRPC env sendNoop (RPC_PING_REQ thatNode)
  pings <- atomically $ readTVar $ pingREQs env
  return (snd (V.head pings) ~= thatNode)

pingsAreRemoved :: IO Bool
pingsAreRemoved = do
  now <- getCurrentTime
  env <- newEnv
  receiveRPC env sendNoop (RPC_PING_REQ thatNode)
  -- go 2 seconds in the future with a threshold of 1 seconds
  pingREQReaperImpl env (addUTCTime 2 now) 1
  pings <- atomically $ readTVar $ pingREQs env
  return $ V.length pings == 0

-- | Have node A store data on node B
storeChunkedData :: IO Bool
storeChunkedData = do
  -- create node B
  envB <- newEnv

  -- node A's ability to send to node B.
  -- whatever it sends will be a noop
  let sendToB = receiveRPC envB sendNoop
  
  -- create node A and give it the key-value pair
  envA <- newEnv
  (dsSet $ dataStore envA) key value

  -- node A stores data on node B
  rpcStore envA sendToB key

  -- and we find the key-value pair in node B's datastore
  mValue <- (dsGet $ dataStore envB) key
  case mValue of
    Just value' | value == value' -> return True
    otherwise -> return False
  where
    key = B.singleton 1
    value = B.pack [1, 2, 3, 4]

