{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Network.Socket
import           System.Environment
import qualified Data.ByteString.Char8 as BC
import qualified Network.Socket.ByteString as NB

-- | IPv4 minimum reassembly buffer size = 576 bytes
-- minus IP header = 20 bytes
-- minus UDP header = 8 bytes
-- == 548 bytes
recvBytes :: Int
recvBytes = 548

main = do
  [port] <- getArgs
  withSocketsDo $ return ()
  addr <- inet_addr "127.0.0.1"
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock $ SockAddrInet (PortNum $ read port) addr
  putStrLn $ "UDP server listening on " ++ port
  forever $ do
    (bs, sockAddr) <- NB.recvFrom sock recvBytes
    putStrLn $ "received [" ++ (BC.unpack bs) ++ "] from " ++ show sockAddr
