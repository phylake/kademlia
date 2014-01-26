{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           GHC.Conc.Sync
import           System.ZMQ4.Monadic
import qualified Data.ByteString.Char8 as BC

sub = runZMQ $ do
  setIoThreads 16
  sockSUB <- socket Sub
  connect sockSUB "tcp://127.0.0.1:3000"
  subscribe sockSUB ""

  sockREQ <- socket Req
  connect sockREQ "tcp://127.0.0.1:3001"
  send sockREQ [] "I'M ALIVE!!!"
  receive sockREQ
  close sockREQ

  forever $ do
    bs <- receive sockSUB
    liftIO $ putStrLn $ BC.unpack bs

main = do
  getNumCapabilities >>= putStrLn . ("num capabilities: "++) . show
  sub
