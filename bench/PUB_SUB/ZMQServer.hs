{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           GHC.Conc.Sync
import           System.ZMQ4
import qualified Data.ByteString.Char8 as BC

sub = do
  ctx <- context
  setIoThreads 16 ctx
  
  sockSUB <- socket ctx Sub
  connect sockSUB "tcp://127.0.0.1:3000"
  subscribe sockSUB ""

  sockREQ <- socket ctx Req
  connect sockREQ "tcp://127.0.0.1:3001"
  send sockREQ [] "I'M ALIVE!!!"
  receive sockREQ
  close sockREQ

  forever $ do
    bs <- receive sockSUB
    putStrLn $ BC.unpack bs

  shutdown ctx

main = do
  getNumCapabilities >>= putStrLn . ("num capabilities: "++) . show
  sub
