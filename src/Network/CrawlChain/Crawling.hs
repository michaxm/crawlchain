{-# LANGUAGE OverloadedStrings #-}
module Network.CrawlChain.Crawling (
  crawl,
  crawlAndStore, CrawlActionDescriber,
  Crawler
) where


import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Http.Client as C
import Network.URI (URI (..), parseURI)

import Network.CrawlChain.CrawlAction
import Network.CrawlChain.CrawlResult
import Network.CrawlChain.Util

type Crawler = CrawlAction -> IO CrawlResult
type CrawlActionDescriber = CrawlAction -> String

{-|
 Processes one step of a crawl chain: does the actual loading.
-}
crawl :: Crawler
crawl action = delaySeconds 1 >> crawlInternal action

{-|
 Used for preparation of integration tests: additionally stores the crawl result
 using the given file name strategy.
-}
crawlAndStore :: CrawlActionDescriber -> Crawler
crawlAndStore describer = (>>= store) . crawl
    where
      store :: CrawlResult -> IO CrawlResult
      store cr = store' (crawlingResultStatus cr)
        where
          store' CrawlingOk = storeResult (crawlingContent cr)
          store' (CrawlingFailed msg) = storeResult msg
          store' status = error $ "unexpected status: " ++ (show status)
          storeResult filecontent'= do
            writeFile' (describer $ crawlingAction cr) filecontent'
            return cr
              where
                writeFile' n c = do
                  putStrLn $ "writing to " ++ n
                  writeFile n c

crawlInternal :: CrawlAction -> IO CrawlResult
crawlInternal action = do
--  print request
  response <- doRequest action
-- print response
  logMsg $ "Crawled " ++ (show action)
  return $ CrawlResult action (BC.unpack response) CrawlingOk
  where
    doRequest :: CrawlAction -> IO (BC.ByteString)
    doRequest (GetRequest url) = C.get (BC.pack url) C.concatHandler -- TODO check exceptions with concatHandler'
    doRequest (PostRequest urlString ps pType) = doPost (BC.pack urlString) formParams pType where
      formParams = map (\(a, b) -> (BC.pack a, BC.pack b)) ps
      doPost :: BC.ByteString -> [(BC.ByteString, BC.ByteString)] -> PostType -> IO BC.ByteString
      doPost url params postType = doPost' postType where
        doPost' :: PostType -> IO BC.ByteString
        doPost' Undefined = doPost' PostForm
        doPost' PostForm = C.postForm url params C.concatHandler
        doPost' PostAJAX = ajaxRequest url params

ajaxRequest :: BC.ByteString -> [(BC.ByteString, BC.ByteString)] -> IO BC.ByteString
ajaxRequest = postRequest C.concatHandler ajaxRequestChanges where
  ajaxRequestChanges = do
    C.setContentType "application/x-www-form-urlencoded; charset=UTF-8"
    C.setAccept "application/json, text/javascript, */*"
    C.setHeader "X-Requested-With" "XMLHttpRequest"
  -- I am not terribly enthusiastic about the http-streams interface when changing headers
  postRequest handler requestChanges url formParams  = do
    bracket
      (C.establishConnection url)
      (C.closeConnection)
      (process)
    where
      u = parseURL url where
        parseURL :: C.URL -> URI
        parseURL r' =
          case parseURI r of
          Just u'  -> u'
          Nothing -> error ("Can't parse URI " ++ r)
          where r = T.unpack $ T.decodeUtf8 r'
      q = C.buildRequest1 $ do
        C.http C.POST (path u)
        C.setAccept $ BC.pack "*/*"
        C.setContentType $ BC.pack "application/x-www-form-urlencoded"
        requestChanges where
          path :: URI -> BC.ByteString
          path u' =
            case url' of
             ""  -> "/"
             _   -> url'
            where
              url' = T.encodeUtf8 $! T.pack $! concat [uriPath u', uriQuery u', uriFragment u']
      process c = do
        _ <- C.sendRequest c q (C.encodedFormBody formParams)
        x <- C.receiveResponse c handler
        return x
