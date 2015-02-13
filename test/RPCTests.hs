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
import qualified Data.Text as T
import qualified Data.Vector as V

rpcSpec :: Spec
rpcSpec = describe "RPC_PING_REQ" $ do
  it "ping pongs" $ do
    pingPong `shouldReturn` Just True

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
