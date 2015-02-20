{-# LANGUAGE RecordWildCards #-}
module RPCTests (rpcSpec) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Network.DHT.Kademlia
import           Network.DHT.Kademlia.Util
import           Network.Socket hiding (send)
import           System.Timeout
import           TestPrelude
import qualified Data.HashTable.IO as H
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Vector as V

rpcSpec :: Spec
rpcSpec = describe "RPCs" $ do
  describe "RPC_PING" $ do
    it "ping pongs" $ do
      pingPong `shouldReturn` Just True
  describe "RPC_STORE" $ do
    it "assembles chunked data" $ do
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

-- | Have node A store data on node B
storeChunkedData :: IO Bool
storeChunkedData = do
  -- create node B
  envB <- newEnv
  let sendFromB = const (return ()) -- ignore input. node B isn't sending anything
  let sendToB = receiveRPC envB sendFromB -- node A's ability to send to node B
  
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

newEnv :: IO KademliaEnv
newEnv = do
  sock <- socket AF_INET Datagram defaultProtocol
  mvStoreHT <- H.new >>= newMVar
  dataStore <- defaultDataStore
  pingREQs <- atomically $ newTVar V.empty
  routingTable <- atomically $ defaultRoutingTable Nothing
  return KademliaEnv{..}
  where
    logDebug _ = return ()
    logInfo _ = return ()
    logWarn _ = return ()
    logError _ = return ()

thisNode :: Node
thisNode = Node 0 (SockAddrInet 3030 2130706433)

thatNode :: Node
thatNode = Node 1 (SockAddrInet 4040 2130706433)
