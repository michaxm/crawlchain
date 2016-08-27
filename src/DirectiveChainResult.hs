module DirectiveChainResult (DirectiveChainResult (..), showResultPath, extractFirstResult) where

import Data.Maybe (listToMaybe)

import CrawlAction
import Report

data DirectiveChainResult = DirectiveChainResult {
                              resultHistory :: [Report],
                              lastResult :: [CrawlAction]
  }

showResultPath :: DirectiveChainResult -> String
showResultPath rp = "next target" ++ (show $ lastResult rp) ++ "\n" ++
                    ((showResult "->" . reverse . resultHistory) rp) ++ "\n" ++
                    "last content:\n" ++ (maybe "<empty" showFullReport $ listToMaybe $ resultHistory rp)
  where
    showResult _ [] = ""
    showResult prefix (x:xs) = prefix ++ (reportMsg x) ++ "\n" ++ (showResult ("  " ++ prefix) xs)

extractFirstResult :: [DirectiveChainResult] -> Maybe CrawlAction
extractFirstResult [] = Nothing
extractFirstResult (r:rs) = let res = lastResult r
                            in if null res then extractFirstResult rs else Just (head res)
