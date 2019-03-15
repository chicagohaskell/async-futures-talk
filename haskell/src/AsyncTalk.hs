{-# LANGUAGE OverloadedStrings #-}

module AsyncTalk where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Char8     (ByteString)
import qualified Data.ByteString.Char8     as BS
import           Network.Socket            hiding (recv)
import           Network.Socket.ByteString
import           Network.URI
import           System.Random             (randomRIO)


data Upstream = Upstream
    { upstreamHost :: HostAddress
    , upstreamPort :: PortNumber
    } deriving (Show, Eq)


localhost :: HostAddress
localhost = tupleToHostAddress (127, 0, 0, 1)


run :: PortNumber -> [Upstream] -> IO ()
run port upstreams = withSocketsDo $ bracket (open localhost port) close (acceptLoop upstreams)


acceptLoop :: [Upstream] -> Socket -> IO ()
acceptLoop upstreams sock = forever $ do
    (conn, peer) <- accept sock
    putStrLn $ "Connection from: " <> show peer
    async $ acceptConn upstreams conn `catch` handleException conn
  where
    handleException :: Socket -> SomeException -> IO ()
    handleException conn e = print e >> close conn


acceptConn :: [Upstream] -> Socket -> IO ()
acceptConn upstreams conn = forever $
    sendAll conn =<< loadBalance upstreams =<< recv conn bufSize


loadBalance :: [Upstream] -> ByteString -> IO ByteString
loadBalance upstreams msg = sendPeer msg =<< oneOf upstreams


-- | Uniformly sample a list
oneOf :: [a] -> IO a
oneOf xs = (xs !!) <$> randomRIO (0, length xs - 1)


sendPeer :: ByteString -> Upstream -> IO ByteString
sendPeer msg (Upstream host port) = do
    sock <- tcpSocket
    connect sock addr
    sendAll sock msg
    recv sock bufSize
  where
    addr = SockAddrInet port host


open :: HostAddress -> PortNumber -> IO Socket
open host port = do
    sock <- tcpSocket
    setSocketOption sock ReuseAddr 1
    -- If the prefork technique is not used,
    -- set CloseOnExec for the security reasons.
    let fd = fdSocket sock
    setCloseOnExecIfNeeded fd
    bind sock bindAddr
    listen sock 10
    return sock
  where
    bindAddr = SockAddrInet port host


tcpSocket :: IO Socket
tcpSocket = socket ipv4AddressFamily Stream tcpProtocolNumber
  where
    tcpProtocolNumber = 6
    ipv4AddressFamily = AF_INET


bufSize :: Int
bufSize = 4096
