module Main where

import           Control.Concurrent
import           Control.Monad
import           Debug.Trace
import           Network.Socket
import qualified Network.Socket.ByteString as NB

main :: IO ()
main = do
  withSocketsDo $ return ()

  sock <- socket AF_INET Datagram defaultProtocol
  addr <- inet_addr "127.0.0.1"
  let sockAddr = SockAddrInet (PortNum 3000) addr
  
  caps <- getNumCapabilities
  
  bind sock sockAddr
  putStrLn "listening on port 3000"
  void $ replicateM (caps - 1) $ forkIO $ loop sock
  loop sock

  return ()

loop sock = forever $ do
  (bs, sockAddr) <- NB.recvFrom sock 548
  traceEventIO "NB.recvFrom"
  return ()
