{-# LANGUAGE DeriveGeneric #-}

module ChatWork.Types.Rooms
    ( GetRoomsResponse
    , GetRoomResponse(..)
    , PostRoomResponse
    , PutRoomResponse
    , GetRoomMembersResponse
    , PutRoomMembersResponse
    , GetRoomMessagesResponse
    , PostRoomMessageResponse
    , GetRoomMessageResponse
    , GetRoomTasksResponse
    , PostRoomTaskResponse
    , GetRoomTaskResponse
    , GetRoomFilesResponse
    , GetRoomFileResponse

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

import ChatWork.Types.Base ( Member, Message, RoomTask, File
                           , IconPreset, TaskStatus
                           , RoomIdWrap, MessageIdWrap, TaskIdsWrap)
import ChatWork.Utils (strLength)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Text (Text)
import GHC.Generics (Generic)

type GetRoomsResponse = [GetRoomResponse]

data GetRoomResponse = GetRoomResponse
                      { getRoomToRoomId :: Int
                      , getRoomToName :: Text
                      , getRoomToType :: Text
                      , getRoomToRole :: Text
                      , getRoomToSticky :: Bool
                      , getRoomToUnreadNum :: Int
                      , getRoomToMentionNum :: Int
                      , getRoomToMytaskNum :: Int
                      , getRoomToMessageNum :: Int
                      , getRoomToFileNum :: Int
                      , getRoomToTaskNum :: Int
                      , getRoomToIconPath :: Text
                      , getRoomToLastUpdateTime :: Int
                      , getRoomToDescription :: Maybe Text
                      } deriving (Show, Generic)

instance ToJSON GetRoomResponse where
  toJSON = genericToJSON $ aesonDrop (strLength "getRoomTo") snakeCase
instance FromJSON GetRoomResponse where
  parseJSON = genericParseJSON $ aesonDrop (strLength "getRoomTo") snakeCase

type PostRoomResponse = RoomIdWrap

type PutRoomResponse = RoomIdWrap

type GetRoomMembersResponse = [Member]

data PutRoomMembersResponse = PutRoomMembersResponse
                            { roomMembersToAdmin :: [Int]
                            , roomMembersToMember :: [Int]
                            , roomMembersToReadonly :: [Int]
                            } deriving (Show, Generic)

instance ToJSON PutRoomMembersResponse where
  toJSON = genericToJSON $ aesonDrop (strLength "roomMembersTo") snakeCase
instance FromJSON PutRoomMembersResponse where
  parseJSON = genericParseJSON $ aesonDrop (strLength "roomMembersTo") snakeCase

type GetRoomMessagesResponse = [Message]

type PostRoomMessageResponse = MessageIdWrap

type GetRoomMessageResponse = Message

type GetRoomTasksResponse = [RoomTask]

type PostRoomTaskResponse = TaskIdsWrap

type GetRoomTaskResponse = RoomTask

type GetRoomFilesResponse = [File]

type GetRoomFileResponse = File

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
