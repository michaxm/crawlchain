module Main (main) where

import System.Exit (exitFailure)

import Network.CrawlChain.CrawlAction
import Text.HTML.CrawlChain.HtmlFiltering

main :: IO ()
main = do
  match extractLinks "<a href=\"link\">text</a>" ["link"]
  match extractLinks "<a href=\"link\" />" ["link"]
  match extractLinks "<a href=\"link\">sdf</a><a href=\"link2\">sdf</a>" ["link", "link2"]
  match (extractLinksFilteringAll unevaluated unevaluated unevaluated) "<a href=\"link\">text<span>bla</span></a>" ["link"]
  match (extractLinksFilteringAll unevaluated unevaluated (any (=="bla"))) "<a href=\"link\">text<span>bla</span></a>" ["link"]
  match (extractLinksFilteringAll unevaluated unevaluated (not . any (=="bla"))) "<a href=\"link\">text<span>bla</span></a>" []
    where
      match method input expected = 
        if actual /= map GetRequest expected
        then putStrLn ("could not find " ++ (show expected) ++ " in; " ++ input) >> exitFailure
        else pure ()
        where
          actual = method input
  
