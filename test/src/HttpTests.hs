module Main (main) where

import System.Exit (exitFailure)

import Network.CrawlChain.CrawlAction
import Network.CrawlChain.CrawlResult
import Network.CrawlChain.Crawling
import Network.URI.Util

main :: IO ()
main = do
  match toURI "http://sdf.sdf/α" (toURI "http://sdf.sdf/α")
  crawlResult <- crawl $ GetRequest "http://example.com/\206\177"
  -- do not care about result, testing for no exceptions with url parsing (yes, doing an external request here is bad style)
  dummyCheck crawlResult
    where
      match method input expected =
        if actual /= expected
        then putStrLn ("could not find " ++ (show expected) ++ " in; " ++ input) >> exitFailure
        else pure ()
        where
          actual = method input
      dummyCheck r =
        if crawlingResultStatus r /= (CrawlingRedirect "sdf")
        then pure ()
        else putStrLn "unexpected" >> exitFailure 
  
