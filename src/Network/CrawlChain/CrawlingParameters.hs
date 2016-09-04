module Network.CrawlChain.CrawlingParameters where

import Network.CrawlChain.CrawlAction
import Network.CrawlChain.CrawlDirective

data CrawlingParameters = CrawlingParameters {
  paramInitialAction :: CrawlAction,
  paramCrawlDirective :: CrawlDirective,
  paramDoDownload :: Bool,
  paramDoStore :: Bool
  }
