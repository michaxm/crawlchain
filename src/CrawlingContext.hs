module CrawlingContext (
    CrawlingContext, DefaultCrawlingContext(..), crawler,
    defaultContext, storingContext, bufferingFilename
  ) where

import Constants
import Crawling (crawl, crawlAndStore, Crawler)
import CrawlAction

class CrawlingContext a where
  crawler :: a -> Crawler

data DefaultCrawlingContext = DefaultCrawlingContext {
  crawlImplementation :: Crawler
}
instance CrawlingContext DefaultCrawlingContext where
  crawler = crawlImplementation

{-
 Enabling tests: provide different crawling implementations:

 - regular
 - storing the crawled URLs and its content to prepare new tests
 - reading the stored content in tests - has no real world application and is defined in the actual tests
-}
defaultContext, storingContext :: DefaultCrawlingContext
defaultContext = DefaultCrawlingContext crawl
storingContext = DefaultCrawlingContext $ crawlAndStore bufferingFilename

bufferingFilename :: CrawlAction -> String
bufferingFilename a = testResourcePath ++ (fname a) ++ if isPost a then "-POST" else ""
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
