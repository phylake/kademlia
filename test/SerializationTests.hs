{-# LANGUAGE OverloadedStrings #-}
module SerializationTests (rpcSerialization) where

import           Data.Binary
import           Network.DHT.Kademlia.Def
import           Network.Socket
import           Test.Hspec
import qualified Data.ByteString as B

rpcSerialization :: Spec
rpcSerialization = describe "RPC serialization" $ do
  rpcPING
  rpcSTORE

rpcSTORE :: Spec
rpcSTORE = describe "STORE" $ do
  it "is isomorphic" $
    encodeDecode rpc `shouldBe` rpc
  it "ignores chunks longer than the specified length" $
    encodeDecode rpcLong `shouldBe` rpc
  where
    rpc :: RPC
    rpc = RPC_STORE key 42 43 12 "chunk o data"
    
    rpcLong :: RPC
    rpcLong = RPC_STORE key 42 43 12 "chunk o data that's longer than expected"

rpcPING :: Spec
rpcPING = describe "PING" $ do
  it "is isomorphic" $
    encodeDecode rpc `shouldBe` rpc
  where
    rpc :: RPC
    rpc = RPC_PING $ Peer 12345678 $ SockAddrInet (PortNum 1024) 0x7F000001
    
    rpcLong :: RPC
    rpcLong = RPC_STORE key 42 43 12 "chunk o data that's longer than expected"

encodeDecode :: (Binary a) => a -> a
encodeDecode = decode . encode

key :: B.ByteString
key = B.pack $ map fromIntegral [0..systemBytes-1]
