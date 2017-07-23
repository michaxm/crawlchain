module Network.CrawlChain.CrawlDirective where

import Network.CrawlChain.CrawlAction
import Network.CrawlChain.CrawlResult

{-|
  A crawl directive takes a content of a web page and produces crawl actions for links/forms to follow.
  The general idea is to specify a list of operations that in theory produces a dynamically collected
  tree of requests which leaves are either dead ends or end results.

  Additional, logical branching/combination of Directives is possible with:
   * Alternatives - evaluate both Directives in order.
   * Restart - evaluate completely new initial action & chain if the previous combo does not produce end results.
-}
data CrawlDirective =
    SimpleDirective (String -> [CrawlAction])
    -- ^ access content to find absolute follow-up urls
  | RelativeDirective (String -> [CrawlAction])
    -- ^ as simple, but found relative urls are completed
  | FollowUpDirective (CrawlResult -> [CrawlAction])
    -- ^ as simple, but with access to complete result
  | DelayDirective Int CrawlDirective
    -- ^ wait additional seconds before executing
  | RetryDirective Int CrawlDirective
    -- ^ if given directive yields no results use add. retries
  | AlternativeDirective CrawlDirective CrawlDirective
    -- ^ fallback to second argument if first yields no results
  | RestartChainDirective (CrawlAction, CrawlDirective)
    -- ^ the possibility to start a new chain (when using alternative)
  | GuardDirective (CrawlAction -> Bool)
    -- ^ not crawling anything, just a blacklisting option
  | DirectiveSequence [CrawlDirective]
    -- ^ chaining of directives
