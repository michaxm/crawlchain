module Network.CrawlChain.CrawlingParameters where

import Network.CrawlChain.CrawlAction
import Network.CrawlChain.CrawlDirective

data CrawlingParameters = CrawlingParameters {
  paramName :: String, -- ^ name of crawl run
  paramInitialAction :: CrawlAction, -- ^ starting point
  paramCrawlDirective :: CrawlDirective, -- ^ list of operations sequentially on all previous results
  paramDoDownload :: Bool, -- ^ store the content of a single result (the first) of the last operation step
  paramDoStore :: Bool -- ^ store the url of a single result (the first)  of the last operation step
  }
