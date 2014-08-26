{-# LANGUAGE OverloadedStrings #-}

module Asset where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Control.Monad (join)
import AudioResource
import EBookResource
import VideoResource
import VideoMashupResource

data Asset = Article !T.Text !T.Text 
           | EBook !T.Text !EBookResource
           | Audio !T.Text AudioResource
           | Video !T.Text VideoResource
           | VideoMashup !T.Text VideoMashupResource deriving Show

buildAsset :: Value -> T.Text -> Parser Asset
buildAsset (Object x) "Audio" = Audio <$> x .: "title" <*> x .: "downloadUrl"
buildAsset (Object x) "Video" = Video <$> x .: "title" <*> x .: "downloadUrl"
buildAsset (Object x) "Article" = Article <$> x .: "title" <*> x .: "description"
buildAsset (Object x) "E-Book" = EBook <$> x .: "title" <*> x .: "downloadUrl"
buildAsset (Object x) "VideoMashup" = VideoMashup <$> x .: "title" <*> x .: "downloadUrl"
buildAsset _ blah = fail ("Failed to parse Asset object!" ++ (T.unpack blah))

instance FromJSON Asset where
  parseJSON o@(Object x) = join $ (buildAsset o) <$> x .: "type"
  parseJSON _ = fail "Failed to parse Asset object"
