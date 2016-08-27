module CrawlChains (
  executeCrawlChain,
  CrawlDirective(..),
  DirectiveChainResult, lastResult, resultHistory, showResultPath,
  (>>+),
  combineAbsoluteUrl)
  where

import Data.List (intersperse)
import Data.List.Split (splitOn)
import System.IO.Unsafe as Unsafe (unsafeInterleaveIO)

import CrawlAction
import CrawlResult
import CrawlingContext
import CrawlDirective
import DirectiveChainResult
import Report


executeCrawlChain :: CrawlingContext a => a -> CrawlAction -> CrawlDirective -> IO [DirectiveChainResult]
executeCrawlChain = followDirective [] []

{-
 Unwraps the actions of the CrawlDirective and applies them, colling the results. Each DirectiveChainResult contains the complete history
 for crawling a chain - either till the end of a directive of a crawl failure. Having multiple in the result is the effect of
 "non-determinism" arising because of possible having multiple follow ups for each crawl.

 Internal signature:
  - collectedResults contains the actual results, updated when actual crawling is done.
  - reportPath contains the current path of the crawling tree, used when adding a new result
 
 Handles the leaves directly, sequences are delegated.
 The simple actions fail, if the previous result was no success. Among these simple directives are used for crawling absolute links,
   followups have to be used to crawl relative links since they allow access to the current url.
 The alternative provides a way to peek, using the second alternative if the first failed.
 The fallback is ignored on success, but on fail initializes the crawl chain anew. This is more powerful than alternatives,
   since the initial action can have a fallback, too.

 Chaining these actions together is done in sequences. Sequences do not do anything on their own (in fact: empty sequences always fail),
   but applies the results of every element to its successors until only one successor remains.
-}
followDirective :: CrawlingContext a =>
                   [DirectiveChainResult] -> [Report] -> a -> CrawlAction -> CrawlDirective
                   -> IO [DirectiveChainResult]
followDirective collectedResults reportPath context crawlAction = followDirective'
  where
    followDirective' :: CrawlDirective -> IO [DirectiveChainResult]
    followDirective' (DirectiveSequence sequence') =
      followDirectiveSequence collectedResults reportPath context crawlAction sequence'
    followDirective' (SimpleDirective logic) = crawlAndSearch (logic . crawlingContent)
    followDirective' (RelativeDirective logic) = crawlAndSearch (makeAbsoluteLogicMapper logic)
    followDirective' (FollowUpDirective logic) = crawlAndSearch logic
    followDirective' (AlternativeDirective a1 a2) = do
      a1Results <- followDirective' a1
      if all (null . lastResult) a1Results -- only use alternative if no results at all
        then followDirective' a2
        else return a1Results
    followDirective' (RestartChainDirective restart) =
      uncurry (followDirective collectedResults reportPath context) restart
    -- actual crawling happens here (and only here):
    crawlAndSearch :: (CrawlResult -> [CrawlAction]) -> IO [DirectiveChainResult]
    crawlAndSearch searchLogic = crawler context crawlAction >>= processCrawlingResult
      where
        processCrawlingResult crawlingResult = return searchCrawlingResult >>= logSearchResults >>= return . wrapActions >>= appendResult
          where
            searchCrawlingResult :: [CrawlAction]
            searchCrawlingResult = if crawlWasNoSuccess crawlingResult then [] else searchLogic crawlingResult
            logSearchResults res = putStrLn ("  found " ++ (show $ length res) ++" follow-up actions:" ++ (show $ map crawlUrl $ res)) >> return res
            wrapActions :: [CrawlAction] -> DirectiveChainResult
            wrapActions res
                | null res = DirectiveChainResult (errReport crawlingResult:reportPath) []
                | otherwise = DirectiveChainResult updateReportPath res
              where
                updateReportPath :: [Report]
                updateReportPath = okReport crawlingResult : reportPath
            appendResult :: DirectiveChainResult -> IO [DirectiveChainResult]
            appendResult = return . (collectedResults ++) . (:[])

{-
 Whereas definition of single directives is pretty straightforward to understand, sequences are handled in this internal method. Notes:

  - sequences itself do not fail (unless defined empty), only directives with actual crawling do
  - sequences do not update the reports or results.
  - crawlingdirectives are trees - primarily not because of sequences or alternatives, but because every simple directive
    may have multiple results - the next directive in a sequence is not a sibling, but the next depth level of the
    crawling tree.
  - however, not all crawling paths must be followed - it is for the caller to decide, how many of the possible leaves are
    actually needed - unused paths should not be actually crawled, the result list needs to be lazy
  - the above also means tree traversal is depth first
  - still, as the following actions depend on the output of the ones before, we need to execute the actions in order when chaining
-}
followDirectiveSequence :: CrawlingContext a => [DirectiveChainResult] -> [Report] -> a -> CrawlAction -> [CrawlDirective] -> IO [DirectiveChainResult]
followDirectiveSequence collectedResults reportPath context crawlingAction' = followDirectiveSequence'
  where
  followDirectiveSequence' :: [CrawlDirective] -> IO [DirectiveChainResult]
  followDirectiveSequence' [] = return [DirectiveChainResult (Report "unsupported: empty sequence" "":reportPath) []]
  followDirectiveSequence' (single:[]) = followDirective collectedResults reportPath context crawlingAction' single
  followDirectiveSequence' (nextDirective:remainingDirectives) = chainDirectives
    where
      -- first things first: this is how the head is processed, its results are not end results, but are input for the next chain step
      -- so the previously collected results are only added, when the chain end is reached (see above)
      nextStepActions :: IO [DirectiveChainResult]
      nextStepActions = followDirective [] reportPath context crawlingAction' nextDirective
      -- this is how one(!) possible follow up has to be calculated
      remainingActions :: [DirectiveChainResult] -> [Report] -> CrawlAction -> IO [DirectiveChainResult]
      remainingActions nextStepResults nextStepReportPath nextStepCrawlAction =
        followDirectiveSequence nextStepResults nextStepReportPath context nextStepCrawlAction remainingDirectives
      chainDirectives :: IO [DirectiveChainResult]
      -- now do the actual crawling of the next step as these results are needed for the following steps
      chainDirectives = do
        nextStepResults <- nextStepActions
        followNextSteps nextStepResults
          where
            followNextSteps :: [DirectiveChainResult] -> IO [DirectiveChainResult]
            followNextSteps nextStepResults = nextStepsResults
              where
                nextStepsResults :: IO [DirectiveChainResult]
                 -- for lazy wrapping of [IO], probably not considered good style
                nextStepsResults = wrapResults $ nextStepsResults' allInputActionsForFollowingSteps
                  where
                    nextStepsResults' :: [CrawlAction] -> [IO [DirectiveChainResult]]
                    nextStepsResults' = map (remainingActions [] reportPath) -- TODO pass previously collected results?
                allInputActionsForFollowingSteps :: [CrawlAction]
                 -- nextStepResults contains the result of the first step as if the chain would stop there
                allInputActionsForFollowingSteps = concat $ map lastResult nextStepResults

wrapResults :: [IO [a]] -> IO [a]
wrapResults = lazyIOsequence -- resorts to unsafe IO internally

-- I do not want that depencency, this is copy/paste (but there should be a "safe" way, right?)
-- | Lazily evaluate each action in the sequence from left to right,
-- and collect the results.
-- PS: also playing around with an additional concat before returning
lazyIOsequence :: [IO [a]] -> IO [a]
lazyIOsequence (mx:mxs) = do
    x   <- mx
    xs  <- Unsafe.unsafeInterleaveIO (Prelude.sequence mxs)
    return $ concat (x : xs)
lazyIOsequence [] = return []


errReport :: CrawlResult -> Report
errReport crawlingResult = report "crawling unsuccessful: " crawlingResult

okReport :: CrawlResult -> Report
okReport crawlingResult = report "ok: " crawlingResult

report :: String -> CrawlResult -> Report
report prefix crawlingResult = Report (prefix ++ (show $ crawlingAction crawlingResult)) (crawlingContent crawlingResult)

crawlWasNoSuccess :: CrawlResult -> Bool
crawlWasNoSuccess = (/= CrawlingOk) . crawlingResultStatus

(>>+) :: (CrawlAction, CrawlDirective) -> CrawlDirective -> (CrawlAction, CrawlDirective)
(>>+) (initialAction, first) second = (initialAction, DirectiveSequence [first, second])

makeAbsoluteLogicMapper :: (String -> [CrawlAction]) -> CrawlResult -> [CrawlAction]
makeAbsoluteLogicMapper logic crawlResult = combineAbsoluteUrls (crawlingAction crawlResult) $ logic $ crawlingContent crawlResult

combineAbsoluteUrls :: CrawlAction -> [CrawlAction] -> [CrawlAction]
combineAbsoluteUrls previousAction = map $ combineAbsoluteUrl previousAction

combineAbsoluteUrl :: CrawlAction -> CrawlAction -> CrawlAction
combineAbsoluteUrl previousAction = addUrlPrefix baseUrl
  where
    baseUrl = ((++"/") . concat . intersperse "/" . dropLast . splitOn "/") (crawlUrl previousAction)
    dropLast es = take (length es -1) es
