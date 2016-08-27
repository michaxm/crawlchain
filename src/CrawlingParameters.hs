module CrawlingParameters where

import CrawlAction
import CrawlDirective

data CrawlingParameters = CrawlingParameters {
  paramInitialAction :: CrawlAction,
  paramCrawlDirective :: CrawlDirective,
  paramDoDownload :: Bool,
  paramDoStore :: Bool
  }
