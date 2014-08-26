{-# LANGUAGE OverloadedStrings #-}

module AudioResource where

import Data.Aeson
import Control.Applicative

data AudioResource = AudioResource { audio :: [String]
                                   , download :: String
                                   } deriving (Show)

instance FromJSON AudioResource where
  parseJSON (Object x) = AudioResource <$> x .: "Audio" <*> x .: "download"
  parseJSON _ = fail "Failed to parse AudioResource object"
