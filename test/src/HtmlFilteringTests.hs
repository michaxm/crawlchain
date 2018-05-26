module Main (main) where

import System.Exit (exitFailure)

import Network.CrawlChain.CrawlAction
import Text.HTML.CrawlChain.HtmlFiltering

main :: IO ()
main = do
  matchR extractLinks "<a href=\"link\">text</a>" ["link"]
  matchR extractLinks "<a href=\"link\" />" ["link"]
  matchR extractLinks "<a href=\"link\">sdf</a><a href=\"link2\">sdf</a>" ["link", "link2"]
  matchR (extractLinksFilteringAll unevaluated unevaluated unevaluated) "<a href=\"link\">text<span>bla</span></a>" ["link"]
  matchR (extractLinksFilteringAll unevaluated unevaluated (any (=="bla"))) "<a href=\"link\">text<span>bla</span></a>" ["link"]
  matchR (extractLinksFilteringAll unevaluated unevaluated (not . any (=="bla"))) "<a href=\"link\">text<span>bla</span></a>" []
  matchR extractLinks "<iframe src=\"link\"></iframe>" ["link"]
  matchR extractLinks "<iframe src=\"link\"/>" ["link"]
  matchR (map GetRequest . findAttributes "attr" . extractTagsContent) "<script attr=\"some_url\">content</div><div>bla</script>" ["some_url"]
  match extractTagsContent "" []
  match extractTagsContent "<div/>" [("div", "", [])]
  match extractTagsContent "<div>sdf</div>" [("div", "sdf", [])]
  match extractTagsContent "<div attr=\"1\">sdf</div>" [("div", "sdf", [("attr", "1")])]
  match extractTagsContent "<script attr=\"some_url\">content</script><div>bla</div>" [("script", "content", [("attr","some_url")]),("div", "bla", [])]
    where
      match method input expected =
        if actual /= expected
        then putStrLn ("could not find " ++ (show expected) ++ " in; " ++ input) >> exitFailure
        else pure ()
        where
          actual = method input
      matchR method input expected = match method input (map GetRequest expected)
  
