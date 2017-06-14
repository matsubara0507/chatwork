{-# LANGUAGE DeriveGeneric #-}

module ChatWork.Types
    ( Token
    , GetMeResponse(..)
    , GetMyStatusResponse(..)
    , GetMyTasksResponse(..)
    , Task(..)
    , Room(..)
    , Account(..)
    ) where

import Data.Aeson
import Data.Aeson.Casing
import Data.Text (Text)
import Data.ByteString (ByteString)
import GHC.Generics

type Token = ByteString

data GetMeResponse = GetMeResponse
                   { meAccountId :: Int
                   , meRoomId :: Int
                   , meName :: Text
                   , meChatworkId :: Text
                   , meOrganizationId :: Int
                   , meOrganizationName :: Text
                   , meDepartment :: Text
                   , meTitle :: Text
                   , meUrl :: Text
                   , meIntroduction :: Text
                   , meMail :: Text
                   , meTelOrganization :: Text
                   , meTelExtension :: Text
                   , meTelMobile :: Text
                   , meSkype :: Text
                   , meFacebook :: Text
                   , meTwitter :: Text
                   , meAvatarImageUrl :: Text
                   } deriving (Show, Generic)

instance ToJSON GetMeResponse where
  toJSON = genericToJSON $ aesonDrop (strLength "me") snakeCase
instance FromJSON GetMeResponse where
  parseJSON = genericParseJSON $ aesonDrop (strLength "me") snakeCase

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

data Task = Task
          { taskToTaskId :: Int
          , taskToRoom :: Room
          , taskToAssignedByAccount :: Account
          , taskToMessageId :: Text
          , taskToBody :: Text
          , taskToLimitTime :: Int
          , taskToStatus :: Text
          } deriving (Show, Generic)

instance ToJSON Task where
  toJSON = genericToJSON $ aesonDrop (strLength "taskTo") snakeCase
instance FromJSON Task where
  parseJSON = genericParseJSON $ aesonDrop (strLength "taskTo") snakeCase

data Room = Room
          { roomToRoomId :: Int
          , roomToName :: Text
          , roomToIconPath :: Text
          } deriving (Show, Generic)

instance ToJSON Room where
  toJSON = genericToJSON $ aesonDrop (strLength "roomTo") snakeCase
instance FromJSON Room where
  parseJSON = genericParseJSON $ aesonDrop (strLength "roomTo") snakeCase

data Account = Account
             { accountToAccountId :: Int
             , accountToName :: Text
             , accountToAvatarImageUrl :: Text
             } deriving (Show, Generic)

instance ToJSON Account where
  toJSON = genericToJSON $ aesonDrop (strLength "accountTo") snakeCase
instance FromJSON Account where
  parseJSON = genericParseJSON $ aesonDrop (strLength "accountTo") snakeCase


strLength :: String -> Int
strLength = length
