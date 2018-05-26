{-# LANGUAGE OverloadedStrings #-}
{-|
 Working with http-streams - this code exists because:
 - there is no simple usage for AJAX
 - its internal parseUrl does not escape and fails if that would have been necessary, a deadly combination for this usage.

 Hoping at least the latter will be fixed upstream.
-}
module Network.Http.ClientFacade (getRequest, ajaxRequest) where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word16)
import qualified Network.Http.Client as C
import qualified Network.URI as U
import OpenSSL.Session (SSLContext)
import qualified System.IO.Unsafe as Unsafe (unsafePerformIO)

import Network.URI.Util

-- reworked from http-streams due to inappropriate escaping/error handling
getRequest :: BC.ByteString -> IO BC.ByteString
getRequest = doRequest C.concatHandler -- TODO check exceptions with concatHandler
  where
    doRequest handler url = do
      bracket
        (openConnection url)
        (C.closeConnection)
        (process)
      where
        u = parseURL url
        q = C.buildRequest1 $ do
          C.http C.GET (path u)
          C.setAccept "*/*" where
            path :: U.URI -> BC.ByteString
            path u' =
              case url' of
              ""  -> "/"
              _   -> url'
              where
                url' = T.encodeUtf8 $! T.pack $! concat [U.uriPath u', U.uriQuery u', U.uriFragment u']
        process c = do
          C.sendRequest c q C.emptyBody
          C.receiveResponse c handler -- wrapRedirect is not exposed: (C.wrapRedirect u 0 handler)

parseURL :: BC.ByteString -> U.URI
parseURL = toURI . T.unpack . T.decodeUtf8

openConnection :: C.URL -> IO C.Connection
--openConnection = C.establishConnection
openConnection = establish . parseURL
  where -- copy/paste from http-streams
    establish u =
      case scheme of
       "http:"  -> do
         C.openConnection host port
       "https:" -> do
         ctx <- readIORef global
         C.openConnectionSSL ctx host ports
--       "unix:"  -> do
--         openConnectionUnix $ U.uriPath u
       _        -> error ("Unknown URI scheme " ++ scheme)
      where
        scheme = U.uriScheme u

        auth = case U.uriAuthority u of
          Just x  -> x
          Nothing -> U.URIAuth "" "localhost" ""

        host = BC.pack (U.uriRegName auth)
        port = case U.uriPort auth of
          ""  -> 80
          _   -> read $ tail $ U.uriPort auth :: Word16
        ports = case U.uriPort auth of
          ""  -> 443
          _   -> read $ tail $ U.uriPort auth :: Word16

        global :: IORef SSLContext
        global = Unsafe.unsafePerformIO $ do
          ctx <- C.baselineContextSSL
          newIORef ctx

ajaxRequest :: BC.ByteString -> [(BC.ByteString, BC.ByteString)] -> IO BC.ByteString
ajaxRequest = postRequest C.concatHandler ajaxRequestChanges where
  ajaxRequestChanges = do
    C.setContentType "application/x-www-form-urlencoded; charset=UTF-8"
    C.setAccept "application/json, text/javascript, */*"
    C.setHeader "X-Requested-With" "XMLHttpRequest"
  -- I am not terribly enthusiastic about the http-streams interface when changing headers
  postRequest handler requestChanges url formParams  = do
    bracket
      (openConnection url)
      (C.closeConnection)
      (process)
    where
      u = parseURL url
      q = C.buildRequest1 $ do
        C.http C.POST (path u)
        C.setAccept $ BC.pack "*/*"
        C.setContentType $ BC.pack "application/x-www-form-urlencoded"
        requestChanges where
          path :: U.URI -> BC.ByteString
          path u' =
            case url' of
             ""  -> "/"
             _   -> url'
            where
              url' = T.encodeUtf8 $! T.pack $! concat [U.uriPath u', U.uriQuery u', U.uriFragment u']
      process c = do
        _ <- C.sendRequest c q (C.encodedFormBody formParams)
        x <- C.receiveResponse c handler
        return x
