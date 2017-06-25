{-# LANGUAGE DeriveGeneric #-}

module ChatWork.Types.My
    ( GetMyStatusResponse(..)
    , GetMyTasksResponse
    ) where

import ChatWork.Types.Base (Task)
import ChatWork.Utils (strLength)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Text (Text)
import GHC.Generics (Generic)

data GetMyStatusResponse = GetMyStatusResponse
                         { myStatusUnreadRoomNum :: Int
                         , myStatusMentionRoomNum :: Int
                         , myStatusMytaskRoomNum :: Int
                         , myStatusUnreadNum :: Int
                         , myStatusMentionNum :: Int
                         , myStatusMytaskNum :: Int
                         } deriving (Show, Generic)

instance ToJSON GetMyStatusResponse where
  toJSON = genericToJSON $ aesonDrop (strLength "myStatus") snakeCase
instance FromJSON GetMyStatusResponse where
  parseJSON = genericParseJSON $ aesonDrop (strLength "myStatus") snakeCase

type GetMyTasksResponse = [Task]
