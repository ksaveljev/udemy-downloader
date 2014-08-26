{-# LANGUAGE OverloadedStrings #-}

module EBookResource where

import Data.Aeson
import Control.Applicative

data EBookResource = EBookResource { eBook :: [String]
                                   , download :: String
                                   } deriving (Show)

instance FromJSON EBookResource where
  parseJSON (Object x) = EBookResource <$> x .: "E-Book" <*> x .: "download"
  parseJSON _ = fail "Failed to parse EBookResource object"
