module Network.CrawlChain.CrawlAction where

data PostType
 = Undefined
 | PostForm
 | PostAJAX
 deriving (Show, Eq)
type PostParams = [(String, String)] 

data CrawlAction = GetRequest String
                 | PostRequest String PostParams PostType
  deriving (Show, Eq)

crawlUrl :: CrawlAction -> String
crawlUrl (GetRequest url) = url
crawlUrl (PostRequest url _ _) = url

{-|
  Adds a prefix to a relative crawl action to get an absolute one.
|-}
addUrlPrefix :: String -> CrawlAction -> CrawlAction
addUrlPrefix p (GetRequest u) = GetRequest (p++u)
addUrlPrefix p (PostRequest u ps t) = PostRequest (p++u) ps t
