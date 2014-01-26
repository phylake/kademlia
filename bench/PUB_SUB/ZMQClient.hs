{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Criterion.Main
import           GHC.Conc.Sync
import           System.ZMQ4

main = do
  ctx <- context
  caps <- getNumCapabilities
  setIoThreads (fromIntegral caps) ctx
  
  sock <- socket ctx Pub
  bind sock "tcp://127.0.0.1:3000"
  
  sockREP <- socket ctx Rep
  bind sockREP "tcp://127.0.0.1:3001"
  receive sockREP
  send sockREP [] "that was dramatic"
  close sockREP

  let io = do {
    send sock [] "data";
  }
  
  defaultMain [ bgroup "zmq PUB-SUB" [
                  bench "only" $ whnfIO io
                ]  
              ]
  close sock
  shutdown ctx
