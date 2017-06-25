{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Types
    ( Token
    , GetMeResponse(..)
    , GetMyStatusResponse(..)
    , GetMyTasksResponse
    , GetContactsResponse
    , GetRoomsResponse
    , PostRoomResponse
    , GetRoomResponse(..)
    , PutRoomResponse
    , GetRoomMembersResponse
    , PutRoomMembersResponse(..)
    , GetIncomingRequestsResponse
    , PutIncomingRequestsResponse(..)
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
    , AccountId
    , CreateUrlFlag
    , MessageBody
    , IconPreset(..)
    , Task(..)
    , Room(..)
    , Account(..)
    , Contact(..)
    , Member(..)
    , Message(..)
    , RoomTask(..)
    , TaskStatus(..)
    , File(..)
    , IncomingRequest(..)
    , RoomIdWrap(..)
    , MessageIdWrap(..)
    , TaskIdsWrap(..)
    , ToReqParam(..)
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Casing
import Data.Monoid (Monoid)
import Data.Text (Text, pack)
import Data.ByteString (ByteString)
import GHC.Generics
import Network.HTTP.Req (QueryParam, (=:))

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

type GetContactsResponse = [Contact]

data Contact = Contact
             { contactToAccountId :: Int
             , contactToRoomId :: Int
             , contactToName :: Text
             , contactToChatworkId :: Text
             , contactToOrganizationId :: Int
             , contactToOrganizationName :: Text
             , contactToDepartment :: Text
             , contactToAvatarImageUrl :: Text
             } deriving (Show, Generic)

instance ToJSON Contact where
  toJSON = genericToJSON $ aesonDrop (strLength "contactTo") snakeCase
instance FromJSON Contact where
  parseJSON = genericParseJSON $ aesonDrop (strLength "contactTo") snakeCase

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

data CreateRoomParams = CreateRoomParams
                      { cRoomDescription :: Maybe Text
                      , cIconPreset :: Maybe IconPreset
                      , cMembersAdminIds :: [Int]
                      , cMembersMemberIds :: Maybe [Int]
                      , cMembersReadonlyIds :: Maybe [Int]
                      , cRoomName :: Text
                      } deriving (Show)

data IconPreset = Group
                | Check
                | Document
                | Meeting
                | Event
                | Project
                | Business
                | Study
                | Security
                | Star
                | Idea
                | Heart
                | Magcup
                | Beer
                | Music
                | Sports
                | Travel
                deriving (Eq)

instance Show IconPreset where
  show Group = "group"
  show Check = "check"
  show Document = "document"
  show Meeting = "meeting"
  show Event = "event"
  show Project = "project"
  show Business = "business"
  show Study = "study"
  show Security = "security"
  show Star = "star"
  show Idea = "idea"
  show Heart = "heart"
  show Magcup = "magcup"
  show Beer = "beer"
  show Music = "music"
  show Sports = "sports"
  show Travel = "travel"

newtype RoomIdWrap = RoomIdWrap { getRoomId :: Int } deriving (Show, Generic)

instance ToJSON RoomIdWrap where
  toJSON = genericToJSON $ aesonDrop (strLength "get") snakeCase
instance FromJSON RoomIdWrap where
  parseJSON = genericParseJSON $ aesonDrop (strLength "get") snakeCase

type PostRoomResponse = RoomIdWrap

type PutRoomResponse = RoomIdWrap

data UpdateRoomParams = UpdateRoomParams
                      { uRoomDescription :: Maybe Text
                      , uIconPreset :: Maybe IconPreset
                      , uRoomName :: Maybe Text
                      } deriving (Show)

data DeleteRoomActionType = LeaveRoom
                          | DeleteRoom
                          deriving (Eq)

instance Show DeleteRoomActionType where
  show LeaveRoom = "leave"
  show DeleteRoom = "delete"

type GetRoomMembersResponse = [Member]

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

data PutRoomMembersResponse = PutRoomMembersResponse
                            { roomMembersToAdmin :: [Int]
                            , roomMembersToMember :: [Int]
                            , roomMembersToReadonly :: [Int]
                            } deriving (Show, Generic)

instance ToJSON PutRoomMembersResponse where
  toJSON = genericToJSON $ aesonDrop (strLength "roomMembersTo") snakeCase
instance FromJSON PutRoomMembersResponse where
  parseJSON = genericParseJSON $ aesonDrop (strLength "roomMembersTo") snakeCase

data RoomMembersParams = RoomMembersParams
                       { getAdminIds :: [Int]
                       , getMemberIds :: Maybe [Int]
                       , getReadonlyIds :: Maybe [Int]
                       } deriving (Show)

type GetRoomMessagesResponse = [Message]

type Force = Bool

type PostRoomMessageResponse = MessageIdWrap

newtype MessageIdWrap = MessageIdWrap { getMessageId :: Text } deriving (Show, Generic)

type MessageBody = Text

instance ToJSON MessageIdWrap where
  toJSON = genericToJSON $ aesonDrop (strLength "get") snakeCase
instance FromJSON MessageIdWrap where
  parseJSON = genericParseJSON $ aesonDrop (strLength "get") snakeCase

type GetRoomMessageResponse = Message

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

type GetRoomTasksResponse = [RoomTask]

data GetTasksParams = GetTasksParams
                   { getTaskAccountId :: Maybe Int
                   , getTaskAssignedByAccountId :: Maybe Int
                   , getTaskStatus :: Maybe TaskStatus
                   } deriving (Show)

data TaskStatus = Open | Done

instance Show TaskStatus where
  show Open = "open"
  show Done = "done"

type PostRoomTaskResponse = TaskIdsWrap

data CreateTaskParams = CreateTaskParams
                      { getTaskBody :: Text
                      , getTaskLimit :: Maybe Int
                      , getTaskToIds :: [Int]
                      } deriving (Show)

type GetRoomTaskResponse = RoomTask

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

type GetRoomFilesResponse = [File]

type AccountId = Int

type GetRoomFileResponse = File

type CreateUrlFlag = Bool

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

type GetIncomingRequestsResponse = [IncomingRequest]

data IncomingRequest = IncomingRequest
                     { incomingRequestToRequestId :: Int
                     , incomingRequestToAccountId :: Int
                     , incomingRequestToMessage :: Text
                     , incomingRequestToName :: Text
                     , incomingRequestToChatworkId :: Text
                     , incomingRequestToOrganizationId :: Int
                     , incomingRequestToOrganizationName :: Text
                     , incomingRequestToDepartment :: Text
                     , incomingRequestToAvatarImageUrl :: Text
                     } deriving (Show, Generic)

instance ToJSON IncomingRequest where
  toJSON = genericToJSON $ aesonDrop (strLength "incomingRequestTo") snakeCase
instance FromJSON IncomingRequest where
  parseJSON = genericParseJSON $ aesonDrop (strLength "incomingRequestTo") snakeCase

data PutIncomingRequestsResponse = IncomingRequestsResponse
                                 { acceptIncomingRequestAccountId :: Int
                                 , acceptIncomingRequestRoomId :: Int
                                 , acceptIncomingRequestName :: Text
                                 , acceptIncomingRequestChatworkId :: Text
                                 , acceptIncomingRequestOrganizationId :: Int
                                 , acceptIncomingRequestOrganizationName :: Text
                                 , acceptIncomingRequestDepartment :: Text
                                 , acceptIncomingRequestAvatarImageUrl :: Text
                                 } deriving (Show, Generic)

instance ToJSON PutIncomingRequestsResponse where
  toJSON = genericToJSON $ aesonDrop (strLength "acceptIncomingRequest") snakeCase
instance FromJSON PutIncomingRequestsResponse where
  parseJSON = genericParseJSON $ aesonDrop (strLength "acceptIncomingRequest") snakeCase

strLength :: String -> Int
strLength = length

class ToReqParam a where
  toReqParam :: (QueryParam param, Monoid param) => Text -> a -> param

instance ToReqParam Int where
  toReqParam = (=:)

instance ToReqParam Text where
  toReqParam = (=:)

instance ToReqParam a => ToReqParam (Maybe a) where
  toReqParam = maybe mempty . toReqParam

instance Show a => ToReqParam [a] where
  toReqParam name = toReqParam name . foldl (\acc a -> mconcat [acc, ",", pack $ show a]) ""

instance ToReqParam IconPreset where
  toReqParam name = toReqParam name . pack . show

instance ToReqParam TaskStatus where
  toReqParam name = toReqParam name . pack . show
