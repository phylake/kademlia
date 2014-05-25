{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module SerializationTests (serialization) where

import           Control.Concurrent.STM
import           Data.Aeson as JSON
import           Data.Binary as Bin
import           Data.Maybe
import           Network.DHT.Kademlia.Def
import           Network.Socket
import           Test.Hspec
import qualified Data.ByteString as B
import qualified Data.Vector as V

serialization :: Spec
serialization = do
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
    node = Node 12345678 $ SockAddrInet (PortNum 1024) 0x7F000001
    
    rpcREQ :: RPC
    rpcREQ = RPC_PING_REQ node

    rpcREP :: RPC
    rpcREP = RPC_PING_REP node

bEncodeDecode :: (Binary a) => a -> a
bEncodeDecode = Bin.decode . Bin.encode

jEncodeDecode :: (ToJSON a, FromJSON a) => a -> a
jEncodeDecode = fromJust . JSON.decode . JSON.encode

key :: B.ByteString
key = B.pack $ map fromIntegral [0..systemBytes-1]
