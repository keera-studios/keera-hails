-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2013
-- License     : BSD3
-- Maintainer  : support@keera.co.uk
module Hails.Network where

import Data.String               (fromString)
import Data.List
import Data.ReactiveValue
import Network.BSD
import Network.Socket
import Network.Socket.ByteString (sendTo)

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
      sendstr omsg = do let bsMsg = fromString omsg
                        sent <- sendTo sock bsMsg (addrAddress serveraddr)
                        sendstr (genericDrop sent omsg)

  return $ ReactiveFieldWrite sendstr
