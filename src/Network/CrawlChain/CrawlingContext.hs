{-|
 Enabling tests: provide different crawling implementations:

 - regular
 - storing the crawled URLs and its content to prepare new tests
 - reading the stored content in tests - has no real world application
-}
module Network.CrawlChain.CrawlingContext (
    CrawlingContext, crawler,
    defaultContext, storingContext, readingContext
  ) where

import System.Directory (doesFileExist)

import Network.CrawlChain.Crawling (crawl, crawlAndStore, Crawler)
import Network.CrawlChain.CrawlAction
import Network.CrawlChain.CrawlResult

class CrawlingContext a where
  crawler :: a -> Crawler

data DefaultCrawlingContext = DefaultCrawlingContext {
  crawlImplementation :: Crawler
}
instance CrawlingContext DefaultCrawlingContext where
  crawler = crawlImplementation

defaultContext :: DefaultCrawlingContext
defaultContext = DefaultCrawlingContext crawl

storingContext :: String -> DefaultCrawlingContext
storingContext prefix = DefaultCrawlingContext $ crawlAndStore $ bufferingFilename prefix

{-|
  Make a unique name for a crawl action - prefix is used to specify the target folder including a specific test prefix
-}
bufferingFilename :: String -> CrawlAction -> String
bufferingFilename prefix a = prefix ++ "/" ++ (fname a) ++ if isPost a then "-POST" else ""
  where
    isPost (PostRequest _ _ _) = True
    isPost _ = False
    fname = lastSegment . crawlUrl
      where
        lastSegment :: String -> String
        lastSegment = reverse . foldl (dropOn '/') []
          where
            dropOn :: Char -> String -> Char -> String
            dropOn c = \collected nextC -> if c == nextC then "" else nextC:collected

readingContext :: String -> DefaultCrawlingContext
readingContext = DefaultCrawlingContext . readFromFiles

readFromFiles :: String -> CrawlAction -> IO CrawlResult
readFromFiles testnamePrefix a = do
  putStrLn $ "  - Reading " ++ filename ++ " for " ++ testnamePrefix
  found <- doesFileExist filename
  if found
    then readFile filename >>= \content -> return $ wrapAsRequest content
    else do
         putStrLn ("  - " ++ notFoundMsg)
         return $ CrawlResult a "" $ CrawlingFailed notFoundMsg
  where
    filename = bufferingFilename testnamePrefix a
    wrapAsRequest :: String -> CrawlResult
    wrapAsRequest content = CrawlResult a content CrawlingOk
    notFoundMsg = "not found in store: " ++ filename
