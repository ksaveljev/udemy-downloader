{-# LANGUAGE OverloadedStrings #-}

module VideoResource where

import Data.Aeson
import Control.Applicative

data VideoResource = VideoResource [String] [String] String deriving (Show)

instance FromJSON VideoResource where
  parseJSON (Object x) = VideoResource <$> x .: "Video" <*> x .: "Video480p" <*> x .: "download"
  parseJSON _ = fail "Failed to parse VideoResource object"
