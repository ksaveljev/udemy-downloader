{-# LANGUAGE OverloadedStrings #-}

module VideoMashupResource where

import Data.Aeson
import Control.Applicative

data VideoMashupResource = VideoMashupResource [String] [String] String [String] deriving (Show)

instance FromJSON VideoMashupResource where
  parseJSON (Object x) = VideoMashupResource <$> x .: "Video" <*> x .: "Video480p" <*> x .: "download" <*> x .: "Presentation"
  parseJSON _ = fail "Failed to parse VideoMashupResource object"
