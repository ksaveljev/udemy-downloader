{-# LANGUAGE OverloadedStrings #-}

module Curriculum where

import qualified Data.Text as T
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Asset
import Control.Monad (join)

type Curriculum = [Content]

data Content = Chapter !T.Text Int
             | Lecture !T.Text Int !T.Text !Asset
             | Quiz !T.Text deriving (Show)

buildContent :: Value -> T.Text -> Parser Content
buildContent (Object x) "lecture" = Lecture <$> x .: "title" <*> x .: "objectIndex" <*> x .: "isDownloadable" <*> x .: "asset"
buildContent (Object x) "chapter" = Chapter <$> x .: "title" <*> x .: "objectIndex"
buildContent (Object x) "quiz" = Quiz <$> x .: "title"
buildContent _ _ = fail "Failed to parse Content object"

instance FromJSON Content where
  parseJSON o@(Object x) = join $ buildContent o <$> x .: "__class"
  parseJSON _ = fail "Failed to parse Content object"
