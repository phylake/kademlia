{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           GHC.Conc.Sync
import           System.ZMQ4
import qualified Data.ByteString.Char8 as BC
  
main = do
  ctx <- context
  setIoThreads 1 ctx
  
  sockSUB <- socket ctx Sub
  connect sockSUB "tcp://127.0.0.1:3000"
  subscribe sockSUB ""

  sockREQ <- socket ctx Req
  connect sockREQ "tcp://127.0.0.1:3001"
  send sockREQ [] ""
  receive sockREQ
  close sockREQ

  loop ctx sockSUB

  shutdown ctx

loop ctx sockSUB = do
  bs <- receive sockSUB
  case bs of
    "exit" -> do
      shutdown ctx
      return ()
    otherwise -> loop ctx sockSUB
