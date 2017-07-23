module Main (main) where

import Data.Maybe (listToMaybe)
import System.Exit (exitFailure)

import Network.CrawlChain
import Network.CrawlChain.CrawlingContext
import Network.CrawlChain.CrawlAction
import Network.CrawlChain.CrawlDirective
import Network.CrawlChain.DirectiveChainResult
import Text.HTML.CrawlChain.HtmlFiltering

testResourcePath :: String
testResourcePath = "test/resources"

-- I really would like to use the detailed test-suite, but that seems to be still unsupported albeit years have passed.
main :: IO ()
main = do
    doTest "basicTest" basicTestChain "expected.html"
  where
    basicTestChain =
      (GetRequest "http://first.html",
       SimpleDirective extractLinks)

doTest :: String -> (CrawlAction, CrawlDirective) -> String -> IO ()
doTest testname template expectedUrl = do
  results <- uncurry (executeCrawlChain testingContext) template
  checkFirstResult results expectedUrl
  where
    testingContext = readingContext $ testResourcePath ++ "/" ++ testname

checkFirstResult :: [DirectiveChainResult] -> String -> IO ()
checkFirstResult rps expected =
  if maybe False (== expected) foundUrl
  then putStrLn $ "\nSuccessfully checked " ++ msg 
  else putStrLn ("\nFailed to check " ++ msg ++ ", found: " ++ (show foundUrl)
                 ++ " in:\n\n" ++ (maybe "" (show . resultHistory) (listToMaybe rps))) >> exitFailure
  where
    foundUrl = extractFirstResult rps >>= return . crawlUrl
    msg = ": " ++ expected
