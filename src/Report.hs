module Report (Report (..), showFullReport) where

data Report = Report { reportMsg :: String, reportDetails :: String }
instance Show Report where
  show = reportMsg

showFullReport :: Report -> String
showFullReport r = reportMsg r ++ "\n\n" ++ (reportDetails r)

