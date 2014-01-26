{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           GHC.Conc.Sync
import           System.Environment
import           System.ZMQ4.Monadic
import qualified Data.ByteString.Char8 as BC

main = do
  caps <- getNumCapabilities
  putStrLn . ("num capabilities: "++) $ show caps
  
  [port] <- getArgs
  let addr = "tcp://127.0.0.1:" ++ port
  putStrLn addr

  runZMQ $ do
    setIoThreads $ fromIntegral caps
    sock <- socket Rep
    bind sock addr
    forever $ do
      bs <- receive sock
      liftIO $ putStrLn $ BC.unpack bs
      send sock [] ""
