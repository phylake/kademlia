{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module SerializationTests (serializationSpec) where

import           Control.Concurrent.STM
import           Data.Aeson as JSON
import           Data.Binary as Bin
import           Data.Maybe
import           Network.DHT.Kademlia.Def
import           Network.Socket
import           Test.Hspec
import qualified Data.ByteString as B
import qualified Data.Vector as V

serializationSpec :: Spec
serializationSpec = do
  routingTableSerialization
  rpcSerialization

routingTableSerialization :: Spec
routingTableSerialization = do
  describe "instance ToJSON/FromJSON RoutingTable" $
    it "is isomorphic" $
      isomorphic `shouldReturn` True
  where
    isomorphic = do
      rt <- atomically $ defaultRoutingTable Nothing >>= V.mapM readTVar
      return $ jEncodeDecode rt == rt

rpcSerialization :: Spec
rpcSerialization = describe "instance Binary RPC" $ do
  rpcPING
  rpcSTORE
  rpcFINDNODE

rpcFINDNODE :: Spec
rpcFINDNODE = describe "FIND_NODE" $ do
  it "REQ is isomorphic" $
    bEncodeDecode rpcREQ `shouldBe` rpcREQ
  it "REP is isomorphic" $
    bEncodeDecode rpcREP `shouldBe` rpcREP
  where
    rpcREQ :: RPC
    rpcREQ = RPC_FIND_NODE_REQ node1 1

    rpcREP :: RPC
    rpcREP = RPC_FIND_NODE_RES node2 1 []

rpcSTORE :: Spec
rpcSTORE = describe "STORE" $ do
  it "is isomorphic" $
    bEncodeDecode rpc `shouldBe` rpc
  it "ignores chunks longer than the specified length" $
    bEncodeDecode rpcLong `shouldBe` rpc
  where
    rpc :: RPC
    rpc = RPC_STORE_REQ key 42 43 12 "chunk o data"
    
    rpcLong :: RPC
    rpcLong = RPC_STORE_REQ key 42 43 12 "chunk o data that's longer than expected"

rpcPING :: Spec
rpcPING = describe "PING" $ do
  it "REQ is isomorphic" $
    bEncodeDecode rpcREQ `shouldBe` rpcREQ
  it "REP is isomorphic" $
    bEncodeDecode rpcREP `shouldBe` rpcREP
  where
    rpcREQ :: RPC
    rpcREQ = RPC_PING_REQ node1

    rpcREP :: RPC
    rpcREP = RPC_PING_RES node1

node1 :: Node
node1 = Node 1 $ SockAddrInet (PortNum 1024) 0x7F000001

node2 :: Node
node2 = Node 2 $ SockAddrInet (PortNum 1024) 0x7F000001

bEncodeDecode :: (Binary a) => a -> a
bEncodeDecode = Bin.decode . Bin.encode

jEncodeDecode :: (ToJSON a, FromJSON a) => a -> a
jEncodeDecode = fromJust . JSON.decode . JSON.encode

key :: B.ByteString
key = B.pack $ map fromIntegral [0..systemBytes-1]
