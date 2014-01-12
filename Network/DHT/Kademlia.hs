{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Network.DHT.Kademlia (runKademlia) where

import           Control.Concurrent
import           Control.Concurrent.MVar (modifyMVar_, newMVar, withMVar, MVar)
import           Control.Concurrent.Suspend.Lifted
import           Control.Concurrent.Timer
import           Control.Monad
import           Data.Binary
import           Data.Word
import           Network.Socket
import           System.Environment
import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import           System.Timeout
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Network.Socket.ByteString as NB

{-
160 bits
k-bucket = list = [(IP, Port, NodeId)]
1 bucket per bit that is of length k
k is some system wide constant
-}

-- | System-wide constant
k :: Int
k = 20

bits :: Int
bits = 3

type NodeId = Integer
type LastSeen = Integer

-- | Sorted by LastSeen
type KBucket = V.Vector (SockAddr, NodeId, LastSeen)

data RPC = RPC_UNKNOWN
         | RPC_PING
         | RPC_STORE
         | RPC_FIND_NODE NodeId
         | RPC_FIND_VALUE
         deriving (Show)

instance Binary RPC where
  put (RPC_PING) = putWord8 1
  put (RPC_STORE) = putWord8 2
  put (RPC_FIND_NODE _) = putWord8 3
  put (RPC_FIND_VALUE) = putWord8 4
  
  get = do
    w <- getWord8
    case w of
      1 -> return $ RPC_PING
      2 -> return $ RPC_STORE
      3 -> return $ RPC_FIND_NODE 0
      4 -> return $ RPC_FIND_VALUE
      otherwise -> return RPC_UNKNOWN

handler :: MVar Int -> IO ()
handler sigMV = modifyMVar_ sigMV (return . (+1))

data Hooks = Hooks {
                     hkPing :: (Int -> IO ())
                   , hkPing2 :: (Int -> IO ())
                   }

type DataStore = (B.ByteString -> IO (Maybe B.ByteString))

-- | IPv4 minimum reassembly buffer size = 576 bytes
-- minus IP header = 20 bytes
-- minus UDP header = 8 bytes
-- == 548 bytes
recvBytes :: Int
recvBytes = 548

runKademlia :: DataStore -> IO ()
runKademlia ds = do
  putStrLn "runKademlia"
  
  (delaySecs:myport:args) <- getArgs
  
  sock <- socket AF_INET Datagram defaultProtocol
  addr <- inet_addr "127.0.0.1"
  let mySockAddr = SockAddrInet (PortNum $ read myport) addr
  bind sock mySockAddr
  
  case args of
    (yourport:[]) -> void $ replicateM 20 $ forkIO $ do
      threadDelay $ secToMicro $ read delaySecs
      let yourSockAddr = SockAddrInet (PortNum $ read yourport) addr
      NB.sendAllTo sock (BL.toStrict $ encode RPC_PING) yourSockAddr
    _ -> return ()
  
  t <- repeatedTimer
    --(NB.sendAll sock $ BL.toStrict $ encode RPC_PING)
    (return ())
    (sDelay $ read delaySecs)
  
  {-sigMV <- newMVar 0
  installHandler sigINT (Catch $ handler sigMV) Nothing
  installHandler sigTERM (Catch $ handler sigMV) Nothing-}

  forever $ do
    (bs, sockAddr) <- NB.recvFrom sock recvBytes
    forkIO $ do
      let send = flip (NB.sendAllTo sock) sockAddr
      let rpc :: RPC = decode $ BL.fromStrict bs
      case rpc of
        RPC_PING -> do
          send "PONG"
          putStrLn $ "received: " ++ (show rpc)
        _ -> do
          putStrLn $ "received: " ++ (BC.unpack bs)
          return ()

  let cleanup = do {
    putStrLn "cleaning up";
    close sock;
    stopTimer t;
  }
    
  --loop sock sigMV cleanup
  return ()

loop :: Socket -> MVar Int -> IO () -> IO ()
loop sock sigMV cleanup = do
  return ()
  --mData <- timeout (secToMicro 4) $ B.recvFrom sock 0x200

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
