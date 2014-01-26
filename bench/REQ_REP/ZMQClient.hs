{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Criterion.Main
import           GHC.Conc.Sync
import qualified System.ZMQ4.Monadic as ZM
import qualified System.ZMQ4 as Z

reqConnectEachTime :: IO ()
reqConnectEachTime = do
  caps <- getNumCapabilities
  void $ ZM.runZMQ $ do
    ZM.setIoThreads (fromIntegral caps)
    sock <- ZM.socket ZM.Req
    ZM.connect sock "tcp://127.0.0.1:3000"
    ZM.send sock [] "data"
    ZM.receive sock

getREQ = do
  ctx <- Z.context
  caps <- getNumCapabilities
  Z.setIoThreads (fromIntegral caps) ctx
  sock <- Z.socket ctx Z.Req
  Z.connect sock "tcp://127.0.0.1:3001"
  let io = do {
    Z.send sock [] "data";
    Z.receive sock
  }
  return (ctx, sock, io)

main = do
  (ctxREQ, sockREQ, connectOnce) <- getREQ
  defaultMain [ bgroup "zmq REQ-REP" [
                  bench "connect each time" reqConnectEachTime
                , bench "connect once" connectOnce
                ]
              ]
  Z.close sockREQ
  Z.shutdown ctxREQ  
