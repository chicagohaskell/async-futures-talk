{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module ChatServer2 where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Char8              (ByteString)
import qualified Data.ByteString.Char8              as BS
import           Data.IORef
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as M
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
    streams <- makeChanPipe
    connState <- newConnectionState
    bracket (open localhost port) close (server streams connState)


data Client = Client
    { clientConn :: Socket
    , clientAddr :: SockAddr
    , toClient   :: OutputStream ByteString
    , toPeers    :: Async ()
    }


newtype ConnectionState = ConnectionState (IORef (Map SockAddr Client))


newConnectionState :: IO ConnectionState
newConnectionState = ConnectionState <$> newIORef mempty


allClients :: ConnectionState -> IO [Client]
allClients (ConnectionState connVar) = M.elems <$> readIORef connVar


addClient :: ConnectionState -> Client -> IO ()
addClient (ConnectionState connVar) client =
    modifyIORef' connVar (M.insert (clientAddr client) client)


server :: (InputStream (SockAddr, ByteString), OutputStream (SockAddr, ByteString)) -> ConnectionState -> Socket -> IO ()
server (fromClients, toClients) connState sock = do
    connAsync <- async $ forever $ acceptClient sock toClients >>= addClient connState
    msgAsync <- async $ forever $ do
        msg <- Streams.read fromClients
        clients <- allClients connState
        traverse (broadcastMsg clients) msg
    waitEither connAsync msgAsync
    putStrLn "Finished!"


acceptClient :: Socket -> OutputStream (SockAddr, ByteString) -> IO Client
acceptClient sock toClients = do
    (conn, peer) <- accept sock
    putStrLn $ "Connection from: " <> show peer
    (fromClient, toClient) <- Streams.socketToStreams conn
    toPeersAsync <- async $ streamClient conn peer fromClient
    return $ Client conn peer toClient toPeersAsync
  where
    streamClient conn peer fromClient =
        (Streams.map (peer,) fromClient >>= Streams.supplyTo toClients)
      `finally`
        close conn >> print (peer, "disconnected")


broadcastMsg :: [Client] -> (SockAddr, ByteString) -> IO ()
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
