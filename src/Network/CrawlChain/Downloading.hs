module Network.CrawlChain.Downloading (downloadTo, storeDownloadAction) where

import qualified Data.ByteString.Char8 as BC
import qualified Network.Http.Client as C

import Network.CrawlChain.CrawlAction
import Network.URI.Util
import Network.CrawlChain.Storing

downloadTo :: Maybe String -> String -> CrawlAction -> IO ()
downloadTo dir destination (GetRequest url) = buildAndCreateTargetDir True dir destination >>= \fulldestination -> do
  Prelude.putStrLn $ "Downloading from "++url++" to "++fulldestination
  downloadResult <- C.get (BC.pack url) C.concatHandler
  BC.writeFile fulldestination downloadResult
  Prelude.putStrLn $ "Download finished"
downloadTo _ _ req = Prelude.putStrLn $ "POST Requests not supported: " ++ show req

storeDownloadAction :: FilePath -> Maybe String -> String -> Maybe CrawlAction -> IO ()
storeDownloadAction storeloc dir destination action = do
  maybe (Prelude.putStrLn "no results found") (go . crawlUrl) action
    where
      go url = do
        Prelude.putStrLn $ "no download requested, appending to " ++ storeloc
        curlCmd <- buildCurlCmd dir destination url
        Prelude.appendFile storeloc curlCmd

{-
downloadStreamingTo :: String -> CrawlAction -> IO ()
downloadStreamingTo destination (GetRequest url) =  do
    req <- parseUrl "https://www.example.com"
    withManager tlsManagerSettings $ \m ->
        withHTTP req m $ \resp ->
            runEffect $ responseBody resp >-> PB.stdout
downloadStreamingTo _ req = Prelude.putStrLn $ "POST Requests not supported: " ++ show req
-}

