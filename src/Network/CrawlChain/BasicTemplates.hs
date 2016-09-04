module Network.CrawlChain.BasicTemplates (
  searchWebTemplate,
  searchWebTemplateAndProcessHits
  ) where

import Data.List (nub, isInfixOf, isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (maybeToList)

import Network.CrawlChain.CrawlAction
import Network.CrawlChain.CrawlResult
import Network.CrawlChain.CrawlDirective
import Network.CrawlChain.CrawlingParameters
import Text.HTML.CrawlChain.HtmlFiltering

-- generic search template

searchWebTemplate :: String -> String -> (CrawlAction, CrawlDirective)
searchWebTemplate site searchTerm = searchWebTemplateAndProcessHits site searchTerm [] Nothing (id . snd)
searchWebTemplateAndProcessHits :: String -> String
                                   -> [String] -> Maybe ContainedTextFilter
                                   -> ((CrawlAction, [CrawlAction]) -> [CrawlAction])
                                   -> (CrawlAction, CrawlDirective)
searchWebTemplateAndProcessHits site searchTerm filterElems tFilter hitProcessor = (
  searchWebAction site searchTerm,
  DirectiveSequence [
    (FollowUpDirective (hitProcessor . extractHits))
  ])
  where
    extractHits :: CrawlResult -> (CrawlAction, [CrawlAction])
    extractHits crawlResult = (crawlingAction crawlResult, extractHits' $ crawlingContent crawlResult)
    extractHits' :: String -> [CrawlAction]
    extractHits' = nub . filterToUrlsContainingAllOf (maybe noTextFilter id tFilter) filterElems

-- using yahoo since the used libraries do not support https, which most other than google/yahoo require
searchWebAction :: String -> String -> CrawlAction
searchWebAction site term = GetRequest $
  "http://us.search.yahoo.com/search?p=" ++ term ++ "+" ++ site

filterToUrlsContainingAllOf :: ContainedTextFilter -> [String] -> String -> [CrawlAction]
filterToUrlsContainingAllOf textFilter = filterToUrlsContainingAllOf'
  where
    filterToUrlsContainingAllOf' [] = filterToUrlsContainingText textFilter ""
    filterToUrlsContainingAllOf' (x:[]) = filterToUrlsContainingText textFilter x
    filterToUrlsContainingAllOf' (x:rest) = (retainActionsContaining x) . (filterToUrlsContainingAllOf' rest)

filterToUrlsContainingText :: ContainedTextFilter -> String -> String -> [CrawlAction]
filterToUrlsContainingText textFilter marker =
  retainActionsContaining marker . extractLinksFilteringAll noUrlFilter noAttrFilter textFilter

filterToUrlsContaining :: String -> String -> [CrawlAction]
filterToUrlsContaining = filterToUrlsContainingText noTextFilter

retainActionsContaining :: String -> [CrawlAction] -> [CrawlAction]
retainActionsContaining marker = filter ((marker `isInfixOf`) . crawlUrl)
