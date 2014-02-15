{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

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
    [] -> usage
    ["--help"] -> usage
    ["gen_conf"] -> return ()
    [cfgFile] -> do
      (ecfg :: Either String Config) <- BL.readFile cfgFile >>= return . eitherDecode
      case ecfg of
        Left err -> putStrLn err >> exitFailure
        Right cfg -> runKademlia cfg
    otherwise -> usage

usage :: IO ()
usage = return ()
