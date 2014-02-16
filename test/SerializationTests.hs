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
    rpc = RPC_STORE_REQ key 42 43 12 "chunk o data"
    
    rpcLong :: RPC
    rpcLong = RPC_STORE_REQ key 42 43 12 "chunk o data that's longer than expected"

rpcPING :: Spec
rpcPING = describe "PING" $ do
  it "REQ is isomorphic" $
    encodeDecode rpcREQ `shouldBe` rpcREQ
  it "REP is isomorphic" $
    encodeDecode rpcREP `shouldBe` rpcREP
  where
    peer = Peer 12345678 $ SockAddrInet (PortNum 1024) 0x7F000001
    
    rpcREQ :: RPC
    rpcREQ = RPC_PING_REQ peer

    rpcREP :: RPC
    rpcREP = RPC_PING_REP peer

encodeDecode :: (Binary a) => a -> a
encodeDecode = decode . encode

key :: B.ByteString
key = B.pack $ map fromIntegral [0..systemBytes-1]
