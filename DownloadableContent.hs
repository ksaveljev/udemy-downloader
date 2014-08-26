module DownloadableContent where

data DownloadableContent =
  DownloadableContent { folder :: String
                      , fileName :: String
                      , downloadUrl :: String
                      } deriving (Show)
