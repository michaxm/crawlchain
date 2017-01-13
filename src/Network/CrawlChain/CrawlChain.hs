module Network.CrawlChain.CrawlChain (
  crawlChain, crawlChains, -- primary interface 
  executeActions, crawlForUrl, -- legacy, deprecated
  executeCrawlChain -- visible for tests
  ) where

import Data.List (intersperse)

import Network.CrawlChain.CrawlAction
import Network.CrawlChain.CrawlingParameters
import Network.CrawlChain.CrawlChains
import Network.CrawlChain.CrawlingContext (defaultContext, storingContext)
import Network.CrawlChain.DirectiveChainResult (extractFirstResult)
import Network.CrawlChain.Downloading

executeActions :: CrawlingParameters -> String -> String -> IO ()
executeActions args dir fName = do
  downloadAction <- crawlChain args
  if paramDoDownload args
    then downloadStep dir fName downloadAction
    else storeDownloadAction "external-load" (Just dir ) fName downloadAction

crawlForUrl :: CrawlingParameters -> IO (Maybe String)
crawlForUrl args = do
  crawlResult <- crawlChain args
  case crawlResult of
       (Just (GetRequest url)) -> return $ Just url
       (Just _) -> putStrLn "POST result processing not implemented" >> return Nothing
       Nothing -> return Nothing

{-|
 Returns only the first result of a completely matching branch of the crawling directive.
-}
crawlChain :: CrawlingParameters -> IO (Maybe CrawlAction)
crawlChain args = do
  results <- crawlChains args
  logAndReturnFirstOk results

{-|
 Returns all possible results of the craling directive - meant to be used with lazyness in mind as needed.
-}
crawlChains :: CrawlingParameters -> IO [DirectiveChainResult]
crawlChains args =
      executeCrawlChain context (paramInitialAction args) (paramCrawlDirective args)
        where
          context = if paramDoStore args then storingContext else defaultContext

downloadStep :: String -> String -> Maybe CrawlAction -> IO ()
downloadStep dir fName downloadAction = maybe (return ()) (downloadTo (Just dir) fName) downloadAction

logAndReturnFirstOk :: [DirectiveChainResult] -> IO (Maybe CrawlAction)
logAndReturnFirstOk results = do
  firstOk <- (return . extractFirstResult) results
  putDetailsOnFailure firstOk results
  return firstOk

putDetailsOnFailure :: Maybe CrawlAction -> [DirectiveChainResult] -> IO ()
putDetailsOnFailure firstSuccess results =
  case firstSuccess of
   Just a -> putStr "  Using result: " >> print a
   Nothing -> do
     putStrLn $ "  No results found - details: " ++ showAllFailures where
       showAllFailures = concat $ intersperse "\n\n" $ map showResultPath results
