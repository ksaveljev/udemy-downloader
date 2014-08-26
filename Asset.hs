{-# LANGUAGE OverloadedStrings #-}

module Asset where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Control.Monad (join)
import qualified AudioResource as AR
import qualified EBookResource as ER
import qualified VideoResource as VR
import qualified VideoMashupResource as VMR

data Asset =
  Article { title :: !T.Text
          , description :: !T.Text
          } |
  EBook { title :: !T.Text
        , eBookResource :: !ER.EBookResource
        } |
  Audio { title :: !T.Text
        , audioResource :: !AR.AudioResource
        } |
  Video { title :: !T.Text
        , videoResource :: !VR.VideoResource
        } |
  VideoMashup { title :: !T.Text
              , videoMashupResource :: !VMR.VideoMashupResource
              } deriving (Show)

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
