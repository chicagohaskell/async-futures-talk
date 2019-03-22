{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module ChatServer2 where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Char8              (ByteString)
import qualified Data.ByteString.Char8              as BS
import           Network.Socket                     hiding (recv, recvFrom)
import           Network.Socket.ByteString
import           Network.URI
import           System.IO.Streams                  (InputStream, OutputStream)
import qualified System.IO.Streams                  as Streams
import           System.IO.Streams.Concurrent.Unagi


localhost :: HostAddress
localhost = tupleToHostAddress (127, 0, 0, 1)


run :: PortNumber -> IO ()
run port = withSocketsDo $ do
    -- streams <- makeChanPipe
    (fromClients, toClients) <- makeChanPipe

    -- Debug
    fromClients' <- Streams.mapM_ (print . ("fromClients",)) fromClients
    toClients' <- Streams.contramapM_ (print . ("toClients",)) toClients
    let streams = (fromClients', toClients')
    -- Debug

    bracket (open localhost port) close (server streams)


data ClientState = ClientState
    { clientConn :: Socket
    , clientAddr :: SockAddr
    , toClient   :: OutputStream ByteString
    , toPeers    :: Async ()
    }


-- TODO First message doesn't seem to get through...
server :: (InputStream (SockAddr, ByteString), OutputStream (SockAddr, ByteString)) -> Socket -> IO ()
server (fromClients, toClients) sock = do
    client <- acceptClientState sock toClients
    go [client]
  where
    go clients = do
        newConn <- async $ acceptClientState sock toClients
        newMsg <- async $ Streams.read fromClients -- probably gets cancelled prematurely
        waitEitherCancel newConn newMsg >>= connectOrMessage clients
    connectOrMessage clients (Left client) = go (client:clients)
    connectOrMessage clients (Right (Just msg)) = do
        broadcastMsg clients msg
        go clients
    connectOrMessage clients (Right Nothing) = return () -- End of stream


acceptClientState :: Socket -> OutputStream (SockAddr, ByteString) -> IO ClientState
acceptClientState sock toClients = do
    (conn, peer) <- acceptClient sock
    (fromClient', toClient') <- Streams.socketToStreams conn

    -- Debug
    fromClient <- Streams.mapM_ (print . ("from", peer,)) fromClient'
    toClient <- Streams.contramapM_ (print . ("to", peer,)) toClient'
    -- Debug

    toPeersAsync <- async $ Streams.map (peer,) fromClient >>= Streams.supplyTo toClients
    return $ ClientState conn peer toClient toPeersAsync
  where
    acceptClient sock = do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from: " <> show peer
        return (conn, peer)


broadcastMsg :: [ClientState] -> (SockAddr, ByteString) -> IO ()
broadcastMsg clients (from, msg) = do
    BS.putStr msg
    mapM_ sendClient clients
  where
    fromMsg = BS.pack (show from) <> ": " <> msg
    sendClient clientState
        | from == clientAddr clientState = return ()
        | otherwise = Streams.write (Just fromMsg) (toClient clientState)


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
    tcpSocket = socket ipv4AddressFamily Stream tcpProtocolNumber
    tcpProtocolNumber = 6
    ipv4AddressFamily = AF_INET
