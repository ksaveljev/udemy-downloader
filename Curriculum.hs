{-# LANGUAGE DeriveGeneric #-}

module Curriculum where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Asset

newtype CourseContent =
  CourseContent { courseContent :: [Curriculum] } deriving (Show, Generic)

data Curriculum =
  Chapter { title :: !T.Text
          , objectIndex :: Int
          } |
  Lecture { title :: !T.Text
          , objectIndex :: Int
          , isDownloadable :: !T.Text
          , asset :: !Asset
          } |
  Quiz { title :: !T.Text
       } deriving (Show, Generic)

instance FromJSON Curriculum
instance FromJSON CourseContent
