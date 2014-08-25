{-# LANGUAGE DeriveGeneric #-}

module Course where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics

data Course =
  Course { title :: !T.Text
         } deriving (Show, Generic)

instance FromJSON Course
