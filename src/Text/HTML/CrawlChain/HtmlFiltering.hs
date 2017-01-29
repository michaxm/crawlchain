module Text.HTML.CrawlChain.HtmlFiltering (
  extractLinks, extractLinksMatching, extractLinksWithAttributes, extractLinksFilteringUrlAttrs, extractLinksFilteringAll, unevaluated,
  findAllUrlsEndingWith, findFirstLinkAfter,
  extractFirstForm,
  Method(..),
  noUrlFilter, AttrFilter, noAttrFilter, ContainedTextFilter, noTextFilter
  ) where

import Data.Char (toLower)
import Data.List (isSuffixOf, isPrefixOf, isInfixOf, nub, sort)
import Data.List.Split (splitOneOf)
import Data.Maybe (fromMaybe)
import Text.HTML.TagSoup

import Network.CrawlChain.CrawlAction

type TagS = Tag String
type AttrFilter = [(String, String)] -> Bool
type ContainedTextFilter = [String] -> Bool

noUrlFilter :: String -> Bool
noUrlFilter = unevaluated

noAttrFilter :: AttrFilter
noAttrFilter = unevaluated

noTextFilter :: ContainedTextFilter
noTextFilter = unevaluated

unevaluated :: a -> Bool
unevaluated _ = True

extractLinks :: String -> [CrawlAction]
extractLinks = extractLinksFilteringUrlAttrs unevaluated unevaluated

extractLinksMatching :: (String -> Bool) -> String -> [CrawlAction]
extractLinksMatching = flip extractLinksFilteringUrlAttrs unevaluated

extractLinksWithAttributes :: AttrFilter -> String -> [CrawlAction]
extractLinksWithAttributes = extractLinksFilteringUrlAttrs unevaluated

extractLinksFilteringUrlAttrs :: (String -> Bool) -> AttrFilter -> String -> [CrawlAction]
extractLinksFilteringUrlAttrs urlFilter attrFilter = extractLinksFilteringAll urlFilter attrFilter noTextFilter

extractLinksFilteringAll :: (String -> Bool) -> AttrFilter -> ContainedTextFilter -> String -> [CrawlAction]
extractLinksFilteringAll urlFilter attrFilter containsTextFilter = extractLinksFiltering combinedFilter
  where
    combinedFilter ts =
      (urlFilter $ getSrc linkTag)
      && (attrFilter $ getTagAttrs linkTag)
      && (containsTextFilter $ getTexts ts)
      where
        linkTag = if null ts then error "empty link tag group" else head ts
        getTexts :: [TagS] -> [String]
        getTexts = filter (not . null) . map (maybe "" id . maybeTagText)

extractLinksFiltering :: ([TagS] -> Bool) -> String -> [CrawlAction]
extractLinksFiltering linkFilter =
  map GetRequest .
  nub .
  filter (not . null) .
  map getSrc .
  map head . filter linkFilter . filter (not . null) .
  filterAndGroupLinks .
  canonicalizeTags .
  parseTags
  where
    filterAndGroupLinks :: [TagS] -> [[TagS]]
    filterAndGroupLinks =
      map cleanupLinkGroup . splitWhen' (\t -> isTagOpenName "a" t || isTagOpenName "iframe" t)
      where
        splitWhen' :: (a -> Bool) -> [a] -> [[a]]
        splitWhen' f = splitWhen'' []
          where
            splitWhen'' col [] = [col]
            splitWhen'' col (x:rest) = if f x then col:(splitWhen'' [x] rest) else splitWhen'' (col++[x]) rest
        cleanupLinkGroup :: [TagS] -> [TagS]
        cleanupLinkGroup = takeWhile notEndSrcTag . dropWhile notStartSrcTag where
          notStartSrcTag t = not $ any (flip isTagOpenName t) supportedTags -- (not . (isTagOpenName "a"))
          notEndSrcTag t = not $ any (flip isTagCloseName t) supportedTags
          supportedTags =  ["a", "iframe"]

findFirstLinkAfter :: String -> [(String, String)] -> String -> [CrawlAction]
findFirstLinkAfter tagName tagAttrs =
  take 1 .
  map GetRequest .
  map getSrc .
  filter isLink .
  dropWhile (not . isMarker) .
  canonicalizeTags .
  parseTags
  where
    isMarker (TagOpen tagName' as) = tagName' == tagName && sort tagAttrs == sort as
    isMarker _ = False
    isLink (TagOpen "a" as) = maybe False (not . null) $ lookup "href" as
    isLink _ = False

getSrc :: Tag String -> String
getSrc (TagOpen _ attributes) = fromMaybe (fromMaybe "" (lookup "src" attributes)) (lookup "href" attributes)
getSrc _ = []

getTagAttrs :: Tag String -> [(String, String)]
getTagAttrs (TagOpen _ as) = as
getTagAttrs _ = []

findAllUrlsEndingWith :: String -> String -> [CrawlAction]
findAllUrlsEndingWith endMarker =
  map GetRequest . filter (endMarker `isSuffixOf`) . filter ("http" `isPrefixOf`) . splitOneOfRetainingNonEmpty " \t\r\n\"\'"

data Method = POST | GET

extractFirstForm :: [String] -> String -> Maybe Method -> String -> String -> Maybe CrawlAction
extractFirstForm extraParams previousUrl method formName content = (buildAction . extractForm method formName [] . parseTags) content
  where
    initialParams = findExtraParams extraParams content
    buildAction :: Maybe [Tag String] -> Maybe CrawlAction
    buildAction = maybe Nothing buildAction'
      where
        buildAction' :: [Tag String] -> Maybe CrawlAction
        buildAction' tags
          | null tags = Nothing
          | not (isFormStart (head tags)) = error $ show tags
          | otherwise = maybe Nothing (buildAction'' (tail tags) initialParams) determineUrl
            where
              determineUrl :: Maybe String
              determineUrl = lookup "action" (tagAttributes (head tags)) >>= (\u -> return (if null u then previousUrl else u))
              buildAction'' :: [Tag String] -> [(String, String)] -> String -> Maybe CrawlAction
              buildAction'' [] params url = Just $ PostRequest url (reverse params) PostForm
              buildAction'' (t:tags') params url = buildAction'' tags' addToParams url
                where
                  addToParams :: [(String, String)]
                  addToParams = maybe params (:params) (extractFormParam t)

isFormStart :: Tag String -> Bool
isFormStart (TagOpen tagName _) = isFormTag tagName
isFormStart _ = False

isFormClose :: Tag String -> Bool
isFormClose (TagClose tagName) = isFormTag tagName
isFormClose _ = False

isFormTag :: String -> Bool
isFormTag = (=="form") . map toLower

isFormStartOf :: Maybe Method -> String -> Tag String -> Bool
isFormStartOf method formName t = isFormStart t
                                  && (null formName || formNameMatches)
                                  && maybe True methodMatches method
  where
    formNameMatches = maybe False (==formName) (getAttributeValue "name")
    methodMatches m = maybe False (methodMatches' m) (getAttributeValue "method")
      where
        methodMatches' POST = (=="POST")
        methodMatches' GET = (=="GET")
    getAttributeValue :: String -> Maybe String
    getAttributeValue = flip lookup (tagAttributes t)

tagAttributes :: Tag String -> [(String, String)]
tagAttributes (TagOpen _ as) = as
tagAttributes _ = []

extractForm :: Maybe Method -> String -> [Tag String] -> [Tag String] -> Maybe [Tag String]
extractForm  _ _ _ [] = Nothing                         -- no complete form
extractForm m n [] (t:tags)
  | isFormStartOf m n t = extractForm m n [t] tags      -- start form element collecting
  | otherwise = extractForm m n [] tags                 -- ... or skip until it starts
extractForm m n collected (t:tags)
  | isFormClose t = Just (reverse collected)            -- first form close, return form (nested forms unsupported)
  | otherwise = extractForm m n (t:collected) tags      -- continue collecting form elements

extractFormParam :: Tag String -> Maybe (String, String)
extractFormParam (TagOpen "input" as) = maybe Nothing (Just . findValueFor) (lookup "name" as)
  where
    findValueFor :: String -> (String, String)
    findValueFor key = (key, fromMaybe [] (lookup "value" as))
extractFormParam _ = Nothing

findExtraParams :: [String] -> String -> [(String, String)]
findExtraParams keys = if null keys then \_ -> [] else findKeyValues
  where
    findKeyValues :: String -> [(String, String)]
    findKeyValues = extractValues . filter containsKey . lines
      where
        containsKey :: String -> Bool
        containsKey line = any (`isInfixOf` line) keys
        extractValues :: [String] -> [(String, String)]
        extractValues = foldr addIfIsValue []
        addIfIsValue :: String -> [(String, String)] -> [(String, String)]
        addIfIsValue line = (values ++)
          where
            splittedLine = splitOneOfRetainingNonEmpty " ,':" line
            values = foldr extractKey [] keys
            extractKey :: String -> [(String, String)] -> [(String, String)]
            extractKey k = (++ (findValue splittedLine))
              where
                findValue :: [String] -> [(String, String)]
                findValue (n':k':v':v'':rest) = if n'=="name" && k'==k && v'=="value" && not (null v'')
                                                then [(k, v'')] else findValue (k':v':v'':rest)
                findValue _ = []

splitOneOfRetainingNonEmpty :: [Char] -> String -> [String]
splitOneOfRetainingNonEmpty splitters = filter (not . null) . splitOneOf splitters
