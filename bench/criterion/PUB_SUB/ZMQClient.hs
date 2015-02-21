{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Criterion.Main
import           GHC.Conc.Sync
import           System.ZMQ4

main = do
  ctx <- context
  caps <- getNumCapabilities
  setIoThreads 1 ctx
  
  sock <- socket ctx Pub
  bind sock "tcp://127.0.0.1:3000"
  
  sockREP <- socket ctx Rep
  bind sockREP "tcp://127.0.0.1:3001"
  receive sockREP
  send sockREP [] ""
  close sockREP

  let io = do {
    send sock [] "datagram";
  }
  
  defaultMain [ bgroup "zmq PUB-SUB" [
                  bench "only" $ whnfIO io
                ]
              ]
  send sock [] "exit"
  close sock
  shutdown ctx
