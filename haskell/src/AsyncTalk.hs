{-# LANGUAGE OverloadedStrings #-}

module AsyncTalk where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Char8     (ByteString)
import qualified Data.ByteString.Char8     as BS
import           Network.HTTP.Base
import           Network.HTTP.Headers
import           Network.Socket            hiding (recv)
import           Network.Socket.ByteString
import           Network.URI


type Application a b = Request a -> IO (Response b)


fileServer :: Application a ByteString
fileServer req = fetchFile `catch` handleError
  where
    -- Warning: Horrendously insecure
    path = "." <> uriPath (rqURI req)

    fetchFile = do
        putStrLn $ "Reading file: " <> path
        fileBody <- BS.readFile path
        return $ okResponse [contentType "text/plain"] fileBody

    handleError :: SomeException -> IO (Response ByteString)
    handleError e = return $ serverErrorResponse [] $
        BS.pack (show e) <> "\nCould not find file: " <> BS.pack path

    okResponse          = Response (2, 0, 0) "OK"
    serverErrorResponse = Response (5, 0, 0) "Internal Server Error"
    contentType = mkHeader HdrContentType


run :: (FromRequest a, ToResponse b) => PortNumber -> Application a b -> IO ()
run port app = withSocketsDo $ bracket (open localhost port) close (acceptLoop app)
  where
    localhost = tupleToHostAddress (127, 0, 0, 1)


runBS :: PortNumber -> Application ByteString ByteString -> IO ()
runBS = run


acceptLoop :: (FromRequest a, ToResponse b) => Application a b -> Socket -> IO ()
acceptLoop app sock = forever $ do
    (conn, peer) <- accept sock
    putStrLn $ "Connection from: " <> show peer
    async $ acceptApp conn app `catch` handleException conn
  where
    handleException :: Socket -> SomeException -> IO ()
    handleException conn e = print e >> close conn


acceptApp :: (FromRequest a, ToResponse b) => Socket -> Application a b -> IO ()
acceptApp conn app = do
    req <- requestParser <$> recv conn 4096
    respond =<< app req
    close conn
  where
    respond = sendAll conn . responseString


open :: HostAddress -> PortNumber -> IO Socket
open host port = do
    sock <- socket ipv4AddressFamily Stream tcpProtocolNumber
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
    tcpProtocolNumber = 6
    ipv4AddressFamily = AF_INET


requestParser :: FromRequest a => ByteString -> Request a
requestParser reqBS = Request u m hdrs bdy
  where
    -- TODO handle parse errors
    (top, bdyBS) = BS.breakSubstring linebreak reqBS
    Right (m, u, hdrs) = parseRequestHead $ lines $ BS.unpack top
    bdy = fromRequestBody $ BS.drop (BS.length linebreak) bdyBS
    linebreak = "\r\n\r\n"


addHeader :: Header -> Response a -> Response a
addHeader hdr resp = resp { rspHeaders = hdr : rspHeaders resp }


responseString :: ToResponse a => Response a -> ByteString
responseString resp = BS.pack (show fmtedResp) <> bdy
  where
    bdy    = toResponseBody (rspBody resp)
    bdyLen = BS.length bdy
    contentLength = addHeader $ mkHeader HdrContentLength $ show bdyLen
    closeResponse = addHeader $ mkHeader HdrConnection "close"
    fmtedResp = closeResponse $ contentLength resp


class FromRequest a where
    fromRequestBody :: ByteString -> a


instance FromRequest ByteString where
    fromRequestBody = id


class ToResponse a where
    toResponseBody :: a -> ByteString


instance ToResponse ByteString where
    toResponseBody = id
