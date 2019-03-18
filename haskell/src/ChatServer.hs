{-# LANGUAGE OverloadedStrings #-}

module ChatServer where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Char8     (ByteString)
import qualified Data.ByteString.Char8     as BS
import           Network.Socket            hiding (recv, recvFrom)
import           Network.Socket.ByteString
import           Network.URI


localhost :: HostAddress
localhost = tupleToHostAddress (127, 0, 0, 1)


run :: PortNumber -> IO ()
run port = withSocketsDo $ bracket (open localhost port) close server


data ClientState = ClientState
    { clientConn :: Socket
    , clientAddr :: SockAddr
    , clientMsg  :: Async (ByteString, SockAddr)
    }


server :: Socket -> IO ()
server sock = do
    client <- newClientState =<< acceptClient sock
    go [client]
  where
    go clients = do
        newClient <- async $ acceptClient sock
        nextMessage <- async $ waitAnyCatch $ map clientMsg clients
        waitEitherCancel newClient nextMessage >>= connectOrMessage clients
    connectOrMessage clients (Right (msgAsync, Right (msg, fromClient))) = do
        broadcastMsg fromClient msg clients
        go =<< replaceClient msgAsync clients
    connectOrMessage clients (Right (msgAsync, Left _)) =
        go =<< removeClient msgAsync clients
    connectOrMessage clients (Left newConn) = do
        client <- newClientState newConn
        go (client:clients)


acceptClient :: Socket -> IO (Socket, SockAddr)
acceptClient sock = do
    (conn, peer) <- accept sock
    putStrLn $ "Connection from: " <> show peer
    return (conn, peer)


newClientState :: (Socket, SockAddr) -> IO ClientState
newClientState (conn, peer) = ClientState conn peer <$> async (recvFrom conn bufSize)
  where bufSize = 4096


replaceClient :: Async (ByteString, SockAddr) -> [ClientState] -> IO [ClientState]
replaceClient msgAsync = mapM replaceMsgAsync
  where
    replaceMsgAsync clientState
        | msgAsync == clientMsg clientState = receiveClient clientState
        | otherwise = return clientState


removeClient :: Async (ByteString, SockAddr) -> [ClientState] -> IO [ClientState]
removeClient msgAsync clients = filterM notFindClient clients
  where
    notFindClient clientState
        | msgAsync == clientMsg clientState = do
            close (clientConn clientState)
            broadcastMsg (clientAddr clientState) "quitting...\n" clients
            return False
        | otherwise = return True


receiveClient :: ClientState -> IO ClientState
receiveClient (ClientState conn peer msgAsync) = do
    cancel msgAsync
    newClientState (conn, peer)


broadcastMsg :: SockAddr -> ByteString -> [ClientState] -> IO ()
broadcastMsg from msg clients = do
    BS.putStrLn fromMsg
    mapM_ sendClient clients
  where
    fromMsg = BS.pack (show from) <> ": " <> msg
    sendClient clientState
        | from == clientAddr clientState = return ()
        | otherwise = sendAll (clientConn clientState) fromMsg


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
