module Network.CrawlChain.Util where

import Prelude hiding (log)

import Control.Concurrent (threadDelay)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)

logMsg :: String -> IO ()
logMsg msg = printTime >> putStr ("> " ++ msg ++ "\n")
  where
    printTime = getZonedTime >>= return . formatTime' >>= putStr
      where
        formatTime' = formatTime defaultTimeLocale "%Y-%m-%d_%H:%M:%S"


delaySeconds :: Int -> IO ()
delaySeconds s = do
  logMsg $ "delaying for "++(show s)++"s"
  threadDelay $ s*1000000
