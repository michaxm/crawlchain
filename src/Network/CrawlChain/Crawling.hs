module Network.CrawlChain.Crawling (
  crawl,
  crawlAndStore, CrawlActionDescriber,
  Crawler
) where

import qualified Data.ByteString.Char8 as BC
import qualified Network.Http.Client as C

import Network.CrawlChain.CrawlAction
import Network.CrawlChain.CrawlResult
import Network.CrawlChain.Util
import Network.Http.ClientFacade

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
    doRequest (GetRequest url) = getRequest (BC.pack url)
    doRequest (PostRequest urlString ps pType) = doPost (BC.pack urlString) formParams pType where
      formParams = map (\(a, b) -> (BC.pack a, BC.pack b)) ps
      doPost :: BC.ByteString -> [(BC.ByteString, BC.ByteString)] -> PostType -> IO BC.ByteString
      doPost url params postType = doPost' postType where
        doPost' :: PostType -> IO BC.ByteString
        doPost' Undefined = doPost' PostForm
        doPost' PostForm = C.postForm url params C.concatHandler
        doPost' PostAJAX = ajaxRequest url params
