module Network.URI.Util (toURI) where

import Data.Maybe (fromMaybe)
import Network.URI

toURI :: String -> URI
toURI url = fromMaybe (error $ "invalid URI - FIXME Network.URI.Util? '" ++ url ++ "'") (parseURI $ escapeURIString isUnescapedInURI url)
