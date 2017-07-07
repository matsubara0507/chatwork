{-# LANGUAGE DeriveGeneric #-}

module ChatWork.Types.Rooms (
    -- Response Types
      Rooms
    , RoomDetail(..)
    , RoomIdWrap(..)
    , Members
    , Member(..)
    , MembersPermission(..)
    , Messages
    , Message(..)
    , MessageIdWrap(..)
    , RoomTasks
    , RoomTask(..)
    , TaskIdsWrap(..)
    , Files
    , File(..)
    -- Request Parameter Types
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

import           ChatWork.Types.Base (Account, IconPreset, TaskStatus)
import           ChatWork.Utils      (strLength)
import           Data.Aeson          (FromJSON (..), ToJSON (..),
                                      genericParseJSON, genericToJSON)
import           Data.Aeson.Casing   (aesonDrop, snakeCase)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)

type Rooms = [RoomDetail]

data RoomDetail = RoomDetail
                { roomDetailToRoomId         :: Int
                , roomDetailToName           :: Text
                , roomDetailToType           :: Text
                , roomDetailToRole           :: Text
                , roomDetailToSticky         :: Bool
                , roomDetailToUnreadNum      :: Int
                , roomDetailToMentionNum     :: Int
                , roomDetailToMytaskNum      :: Int
                , roomDetailToMessageNum     :: Int
                , roomDetailToFileNum        :: Int
                , roomDetailToTaskNum        :: Int
                , roomDetailToIconPath       :: Text
                , roomDetailToLastUpdateTime :: Int
                , roomDetailToDescription    :: Maybe Text
                } deriving (Eq, Show, Generic)

instance ToJSON RoomDetail where
  toJSON = genericToJSON $ aesonDrop (strLength "roomDetailTo") snakeCase
instance FromJSON RoomDetail where
  parseJSON = genericParseJSON $ aesonDrop (strLength "roomDetailTo") snakeCase

newtype RoomIdWrap = RoomIdWrap { getRoomId :: Int } deriving (Eq, Show, Generic)

instance ToJSON RoomIdWrap where
  toJSON = genericToJSON $ aesonDrop (strLength "get") snakeCase
instance FromJSON RoomIdWrap where
  parseJSON = genericParseJSON $ aesonDrop (strLength "get") snakeCase

type Members = [Member]

data Member = Member
            { memberToAccountId        :: Int
            , memberToRole             :: Text
            , memberToName             :: Text
            , memberToChatworkId       :: Text
            , memberToOrganizationId   :: Int
            , memberToOrganizationName :: Text
            , memberToDepartment       :: Text
            , memberToAvatarImageUrl   :: Text
            } deriving (Eq, Show, Generic)

instance ToJSON Member where
  toJSON = genericToJSON $ aesonDrop (strLength "memberTo") snakeCase
instance FromJSON Member where
  parseJSON = genericParseJSON $ aesonDrop (strLength "memberTo") snakeCase

data MembersPermission = MembersPermission
                 { membersPermissionToAdmin    :: [Int]
                 , membersPermissionToMember   :: [Int]
                 , membersPermissionToReadonly :: [Int]
                 } deriving (Eq, Show, Generic)

instance ToJSON MembersPermission where
  toJSON = genericToJSON $ aesonDrop (strLength "membersPermissionTo") snakeCase
instance FromJSON MembersPermission where
  parseJSON = genericParseJSON $ aesonDrop (strLength "membersPermissionTo") snakeCase

type Messages = [Message]

data Message = Message
             { messageToMessageId  :: Text
             , messageToAccount    :: Account
             , messageToBody       :: Text
             , messageToSendTime   :: Int
             , messageToUpdateTime :: Int
             } deriving (Eq, Show, Generic)

instance ToJSON Message where
  toJSON = genericToJSON $ aesonDrop (strLength "messageTo") snakeCase
instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonDrop (strLength "messageTo") snakeCase

newtype MessageIdWrap = MessageIdWrap { getMessageId :: Text } deriving (Eq, Show, Generic)

instance ToJSON MessageIdWrap where
  toJSON = genericToJSON $ aesonDrop (strLength "get") snakeCase
instance FromJSON MessageIdWrap where
  parseJSON = genericParseJSON $ aesonDrop (strLength "get") snakeCase

type RoomTasks = [RoomTask]

data RoomTask = RoomTask
              { roomTaskToTaskId            :: Int
              , roomTaskToAccount           :: Account
              , roomTaskToAssignedByAccount :: Account
              , roomTaskToMessageId         :: Text
              , roomTaskToBody              :: Text
              , roomTaskToLimitTime         :: Int
              , roomTaskToStatus            :: Text
              } deriving (Eq, Show, Generic)

instance ToJSON RoomTask where
  toJSON = genericToJSON $ aesonDrop (strLength "roomTaskTo") snakeCase
instance FromJSON RoomTask where
  parseJSON = genericParseJSON $ aesonDrop (strLength "roomTaskTo") snakeCase

newtype TaskIdsWrap = TaskIdsWrap { getTaskIds :: [Int] } deriving (Eq, Show, Generic)

instance ToJSON TaskIdsWrap where
  toJSON = genericToJSON $ aesonDrop (strLength "get") snakeCase
instance FromJSON TaskIdsWrap where
  parseJSON = genericParseJSON $ aesonDrop (strLength "get") snakeCase

type Files = [File]

data File = File
          { fileToFileId     :: Int
          , fileToAccount    :: Account
          , fileToMessageId  :: Text
          , fileToFilename   :: Text
          , fileToFilesize   :: Int
          , fileToUploadTime :: Int
          } deriving (Eq, Show, Generic)

instance ToJSON File where
  toJSON = genericToJSON $ aesonDrop (strLength "fileTo") snakeCase
instance FromJSON File where
  parseJSON = genericParseJSON $ aesonDrop (strLength "fileTo") snakeCase

-- |
-- see: <http://developer.chatwork.com/ja/endpoint_rooms.html#POST-rooms>
data CreateRoomParams = CreateRoomParams
                      { cRoomDescription    :: Maybe Text
                      , cIconPreset         :: Maybe IconPreset
                      , cMembersAdminIds    :: [Int]
                      , cMembersMemberIds   :: Maybe [Int]
                      , cMembersReadonlyIds :: Maybe [Int]
                      , cRoomName           :: Text
                      } deriving (Show)

-- |
-- see: <http://developer.chatwork.com/ja/endpoint_rooms.html#PUT-rooms-room_id>
data UpdateRoomParams = UpdateRoomParams
                      { uRoomDescription :: Maybe Text
                      , uIconPreset      :: Maybe IconPreset
                      , uRoomName        :: Maybe Text
                      } deriving (Show)

-- |
-- see: <http://developer.chatwork.com/ja/endpoint_rooms.html#PUT-rooms-room_id-members>
data RoomMembersParams = RoomMembersParams
                       { getAdminIds    :: [Int]
                       , getMemberIds   :: Maybe [Int]
                       , getReadonlyIds :: Maybe [Int]
                       } deriving (Show)

-- |
-- see: <http://developer.chatwork.com/ja/endpoint_rooms.html#GET-rooms-room_id-tasks>
data GetTasksParams = GetTasksParams
                   { getTaskAccountId           :: Maybe Int
                   , getTaskAssignedByAccountId :: Maybe Int
                   , getTaskStatus              :: Maybe TaskStatus
                   } deriving (Show)

-- |
-- see: <http://developer.chatwork.com/ja/endpoint_rooms.html#POST-rooms-room_id-tasks>
data CreateTaskParams = CreateTaskParams
                      { getTaskBody  :: Text
                      , getTaskLimit :: Maybe Int
                      , getTaskToIds :: [Int]
                      } deriving (Show)

-- |
-- see: <http://developer.chatwork.com/ja/endpoint_rooms.html#DELETE-rooms-room_id>
data DeleteRoomActionType = LeaveRoom
                          | DeleteRoom
                          deriving (Eq)

instance Show DeleteRoomActionType where
  show LeaveRoom  = "leave"
  show DeleteRoom = "delete"

-- |
-- see: <http://developer.chatwork.com/ja/endpoint_rooms.html#GET-rooms-room_id-messages>
type Force = Bool

-- |
-- see: <http://developer.chatwork.com/ja/endpoint_rooms.html#POST-rooms-room_id-messages>
type MessageBody = Text

-- |
-- see: <http://developer.chatwork.com/ja/endpoint_rooms.html#GET-rooms-room_id-files-file_id>
type CreateUrlFlag = Bool
