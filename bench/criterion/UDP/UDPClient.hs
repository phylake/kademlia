{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Criterion.Main
import           Network.Socket
import qualified Network.Socket.ByteString as NB

main = do
  withSocketsDo $ return ()
  addr <- inet_addr "127.0.0.1"
  
  let connectSockAddr = SockAddrInet (PortNum 3001) addr
  connectSock <- socket AF_INET Datagram defaultProtocol
  let connectAndSend = do {
    connected <- isConnected connectSock;
    if connected then return () else connect connectSock connectSockAddr;
    NB.send connectSock "datagram";
  }

  sendtoSock <- socket AF_INET Datagram defaultProtocol
  let sendtoSockAddr = SockAddrInet (PortNum 3002) addr
  let sendto = do {
    NB.sendTo sendtoSock "datagram" sendtoSockAddr;
  }
  
  defaultMain [ bgroup "udp" [
                  bench "connect and send" $ whnfIO connectAndSend
                , bench "sendto" $ whnfIO sendto
                ]
              ]
  NB.sendTo sendtoSock "exit" sendtoSockAddr
  NB.send connectSock "exit"
