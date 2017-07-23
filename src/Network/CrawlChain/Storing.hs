module Network.CrawlChain.Storing where

import System.Directory

buildCurlCmd :: Maybe String -> String -> String -> IO String
buildCurlCmd dir destination url = do
  targetPath <- buildAndCreateTargetDir False dir destination
  return $ curlLoadAction targetPath url
    where
      curlLoadAction targetPath url = "curl -s -o " ++ targetPath ++ " " ++ url ++ "\n"

buildAndCreateTargetDir :: Bool -> Maybe String -> String -> IO String
buildAndCreateTargetDir createDir dir dest = do
  maybe
   (return dest)
   (\dir' -> do
       if createDir
         then createDirectoryIfMissing True dir'
         else return ()
       return (dir' ++ "/" ++ dest)
       )
   dir
