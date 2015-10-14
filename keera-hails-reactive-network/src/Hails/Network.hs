module Hails.Network where

import Data.List
import Data.ReactiveValue
import Network.BSD
import Network.Socket

-- | Create a UDP sink (a write-only reactive value).
udpSink :: HostName -> String -> IO (ReactiveFieldWrite IO String)
udpSink hostname port = do
  -- Obtain server addr
  addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = head addrinfos

  -- Establish a socket for communication
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

  -- Send command
  let sendstr :: String -> IO ()
      sendstr []   = return ()
      sendstr omsg = do sent <- sendTo sock omsg (addrAddress serveraddr)
                        sendstr (genericDrop sent omsg)

  return $ ReactiveFieldWrite sendstr
