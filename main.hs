{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import           Data.Aeson
import           Network.DHT.Kademlia
import           Network.DHT.Kademlia.Def (Config(..))
import           System.Environment (getArgs)
import           System.Exit
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    ["gen_conf"] -> do
      putStrLn $ unlines [
          "{"
        , "  \"routingTablePath\": \"routes.json\","
        , "  \"datastore\": {"
        , "    \"type\": \"hashtables\""
        , "  },"
        , "  \"seedNode\": {"
        , "    \"nodeId\": 2.1111456317203406E+38,"
        , "    \"location\": {"
        , "      \"port\": 1234,"
        , "      \"host\": 16777343"
        , "    }"
        , "  },"
        , "  \"thisNode\": {"
        , "    \"nodeId\": 2.3931456317203406E+38,"
        , "    \"location\": {"
        , "      \"port\": 41487,"
        , "      \"host\": 16777343"
        , "    }"
        , "  }"
        , "}"
        ]
    [cfgFile] -> do
      (ecfg :: Either String Config) <- liftM eitherDecode $ BL.readFile cfgFile
      case ecfg of
        Left err -> putStrLn err >> exitFailure
        Right cfg -> runKademlia cfg
    otherwise -> usage

usage :: IO ()
usage = do
  putStrLn ""
  exitFailure

