module Network.CrawlChain.CrawlChain
       (executeActions, crawlChain, crawlForUrl,
        executeCrawlChain -- visible for tests
       )
       where

import Data.List (intersperse)
import Data.List.Split (splitOn)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import Network.CrawlChain.CrawlAction
import Network.CrawlChain.CrawlingParameters
import Network.CrawlChain.CrawlChains
import Network.CrawlChain.CrawlingContext (defaultContext, storingContext)
import Network.CrawlChain.DirectiveChainResult (extractFirstResult)
import Network.CrawlChain.Downloading
import Network.CrawlChain.Storing
import Network.CrawlChain.BasicTemplates

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

crawlChain :: CrawlingParameters -> IO (Maybe CrawlAction)
crawlChain args = do
  results <- crawlingChain
  logAndReturnFirstOk results
  where
    crawlingChain :: IO [DirectiveChainResult]
    crawlingChain =
      executeCrawlChain context (paramInitialAction args) (paramCrawlDirective args)
        where
          context = if paramDoStore args then storingContext else defaultContext

downloadStep :: String -> String -> Maybe CrawlAction -> IO ()
downloadStep dir fName downloadAction = maybe (return ()) (downloadTo (Just dir) fName) downloadAction

logAndReturnFirstOk :: [DirectiveChainResult] -> IO (Maybe CrawlAction)
logAndReturnFirstOk results = do
  firstOk <- (return . extractFirstResult) results
  putResults firstOk results
  return firstOk

putResults :: Maybe CrawlAction -> [DirectiveChainResult] -> IO ()
putResults firstSuccess results =
  maybe writeFailures putSuccess firstSuccess
    where
      putSuccess :: CrawlAction -> IO ()
      putSuccess a = putStrLn (crawlUrl a)
      writeFailures = do
        putStrLn "no results found, look into failures.log"
        writeFile "failures.log" showAllFailures
          where showAllFailures = concat $ intersperse "\n\n" $ map showResultPath results
