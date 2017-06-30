{-# LANGUAGE DeriveGeneric #-}

module ChatWork.Types.Rooms
    ( Rooms
    , RoomDetail(..)
    , RoomIdWrap(..)
    , Members
    , Member(..)
    , PermissionMembers(..)
    , Messages
    , Message(..)
    , MessageIdWrap(..)
    , RoomTasks
    , RoomTask
    , TaskIdsWrap(..)
    , Files
    , File(..)

    , CreateRoomParams(..)
    , UpdateRoomParams(..)
    , RoomMembersParams(..)
    , GetTasksParams(..)
    , CreateTaskParams(..)
    , DeleteRoomActionType(..)
    , Force
    , MessageBody
    , CreateUrlFlag
    ) where

import ChatWork.Types.Base (Account, IconPreset, TaskStatus)
import ChatWork.Utils (strLength)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Text (Text)
import GHC.Generics (Generic)

type Rooms = [RoomDetail]

data RoomDetail = RoomDetail
                { roomDetailToRoomId :: Int
                , roomDetailToName :: Text
                , roomDetailToType :: Text
                , roomDetailToRole :: Text
                , roomDetailToSticky :: Bool
                , roomDetailToUnreadNum :: Int
                , roomDetailToMentionNum :: Int
                , roomDetailToMytaskNum :: Int
                , roomDetailToMessageNum :: Int
                , roomDetailToFileNum :: Int
                , roomDetailToTaskNum :: Int
                , roomDetailToIconPath :: Text
                , roomDetailToLastUpdateTime :: Int
                , roomDetailToDescription :: Maybe Text
                } deriving (Show, Generic)

instance ToJSON RoomDetail where
  toJSON = genericToJSON $ aesonDrop (strLength "roomDetailTo") snakeCase
instance FromJSON RoomDetail where
  parseJSON = genericParseJSON $ aesonDrop (strLength "roomDetailTo") snakeCase

newtype RoomIdWrap = RoomIdWrap { getRoomId :: Int } deriving (Show, Generic)

instance ToJSON RoomIdWrap where
  toJSON = genericToJSON $ aesonDrop (strLength "get") snakeCase
instance FromJSON RoomIdWrap where
  parseJSON = genericParseJSON $ aesonDrop (strLength "get") snakeCase

type Members = [Member]

data Member = Member
            { memberToAccountId :: Int
            , memberToRole :: Text
            , memberToName :: Text
            , memberToChatworkId :: Text
            , memberToOrganizationId :: Int
            , memberToOrganizationName :: Text
            , memberToDepartment :: Text
            , memberToAvatarImageUrl :: Text
            } deriving (Show, Generic)

instance ToJSON Member where
  toJSON = genericToJSON $ aesonDrop (strLength "memberTo") snakeCase
instance FromJSON Member where
  parseJSON = genericParseJSON $ aesonDrop (strLength "memberTo") snakeCase

data PermissionMembers = PermissionMembers
                 { permissionMembersToAdmin :: [Int]
                 , permissionMembersToMember :: [Int]
                 , permissionMembersToReadonly :: [Int]
                 } deriving (Show, Generic)

instance ToJSON PermissionMembers where
  toJSON = genericToJSON $ aesonDrop (strLength "permissionMembersTo") snakeCase
instance FromJSON PermissionMembers where
  parseJSON = genericParseJSON $ aesonDrop (strLength "permissionMembersTo") snakeCase

type Messages = [Message]

data Message = Message
             { messageToMessageId :: Text
             , messageToAccount :: Account
             , messageToBody :: Text
             , messageToSendTime :: Int
             , messageToUpdateTime :: Int
             } deriving (Show, Generic)

instance ToJSON Message where
  toJSON = genericToJSON $ aesonDrop (strLength "messageTo") snakeCase
instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonDrop (strLength "messageTo") snakeCase

newtype MessageIdWrap = MessageIdWrap { getMessageId :: Text } deriving (Show, Generic)

instance ToJSON MessageIdWrap where
  toJSON = genericToJSON $ aesonDrop (strLength "get") snakeCase
instance FromJSON MessageIdWrap where
  parseJSON = genericParseJSON $ aesonDrop (strLength "get") snakeCase

type RoomTasks = [RoomTask]

data RoomTask = RoomTask
              { roomTaskToTaskId :: Int
              , roomTaskToAccount :: Account
              , roomTaskToAssignedByAccount :: Account
              , roomTaskToMessageId :: Text
              , roomTaskToBody :: Text
              , roomTaskToLimitTime :: Int
              , roomTaskToStatus :: Text
              } deriving (Show, Generic)

instance ToJSON RoomTask where
  toJSON = genericToJSON $ aesonDrop (strLength "roomTaskTo") snakeCase
instance FromJSON RoomTask where
  parseJSON = genericParseJSON $ aesonDrop (strLength "roomTaskTo") snakeCase

newtype TaskIdsWrap = TaskIdsWrap { getTaskIds :: [Int] } deriving (Show, Generic)

instance ToJSON TaskIdsWrap where
  toJSON = genericToJSON $ aesonDrop (strLength "get") snakeCase
instance FromJSON TaskIdsWrap where
  parseJSON = genericParseJSON $ aesonDrop (strLength "get") snakeCase

type Files = [File]

data File = File
          { fileToFileId :: Int
          , fileToAccount :: Account
          , fileToMessageId :: Text
          , fileToFilename :: Text
          , fileToFilesize :: Int
          , fileToUploadTime :: Int
          } deriving (Show, Generic)

instance ToJSON File where
  toJSON = genericToJSON $ aesonDrop (strLength "fileTo") snakeCase
instance FromJSON File where
  parseJSON = genericParseJSON $ aesonDrop (strLength "fileTo") snakeCase

data CreateRoomParams = CreateRoomParams
                      { cRoomDescription :: Maybe Text
                      , cIconPreset :: Maybe IconPreset
                      , cMembersAdminIds :: [Int]
                      , cMembersMemberIds :: Maybe [Int]
                      , cMembersReadonlyIds :: Maybe [Int]
                      , cRoomName :: Text
                      } deriving (Show)

data UpdateRoomParams = UpdateRoomParams
                      { uRoomDescription :: Maybe Text
                      , uIconPreset :: Maybe IconPreset
                      , uRoomName :: Maybe Text
                      } deriving (Show)

data RoomMembersParams = RoomMembersParams
                       { getAdminIds :: [Int]
                       , getMemberIds :: Maybe [Int]
                       , getReadonlyIds :: Maybe [Int]
                       } deriving (Show)

data GetTasksParams = GetTasksParams
                   { getTaskAccountId :: Maybe Int
                   , getTaskAssignedByAccountId :: Maybe Int
                   , getTaskStatus :: Maybe TaskStatus
                   } deriving (Show)

data CreateTaskParams = CreateTaskParams
                      { getTaskBody :: Text
                      , getTaskLimit :: Maybe Int
                      , getTaskToIds :: [Int]
                      } deriving (Show)

data DeleteRoomActionType = LeaveRoom
                          | DeleteRoom
                          deriving (Eq)

instance Show DeleteRoomActionType where
  show LeaveRoom = "leave"
  show DeleteRoom = "delete"

type Force = Bool

type MessageBody = Text

type CreateUrlFlag = Bool
