{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Asset where

import qualified Data.Text as T
import Data.Aeson
import Control.Applicative
import GHC.Generics

data Asset =
  Asset { assetType :: !T.Text
        , title :: !T.Text
        , downloadUrl :: !Downloadable
        } deriving (Show)

data Downloadable =
  Downloadable { download :: String
               } deriving (Show, Generic)

instance FromJSON Asset where
  parseJSON (Object x) = Asset <$> x .: "type" <*> x .: "title" <*> x .: "downloadUrl"
  parseJSON _ = fail "Failed to parse Asset object"

instance FromJSON Downloadable
