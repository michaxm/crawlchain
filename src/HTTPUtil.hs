module HTTPUtil (toURI) where

import Data.Maybe (fromMaybe)
import Network.URI

toURI :: String -> URI
toURI url = fromMaybe (error $ "invalid URI '" ++ url ++ "'") (parseURI url)
