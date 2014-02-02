{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Network.Socket
import qualified Data.ByteString as B
import qualified Network.Socket.ByteString as NB

threads = 10000

main :: IO ()
main = do
  withSocketsDo $ return ()

  sock <- socket AF_INET Datagram defaultProtocol
  addr <- inet_addr "127.0.0.1"
  let sockAddr = SockAddrInet (PortNum 3000) addr
  
  mv <- newMVar 0
  void . replicateM threads . forkIO $ do
    forM_ [0..100] $ \i -> do
      NB.sendAllTo sock (ping i) sockAddr
    c <- takeMVar mv
    putMVar mv $ c+1
  wait mv
  return ()
  where
    wait mv = do
      c <- readMVar mv
      if c == threads
        then do
          putStrLn $ show threads
          return ()
        else wait mv

    ping i = B.append "PING" (B.singleton $ fromIntegral i)
