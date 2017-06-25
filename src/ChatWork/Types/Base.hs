{-# LANGUAGE DeriveGeneric #-}

module ChatWork.Types.Base
    ( Task(..)
    , Room(..)
    , Account(..)
    , Contact(..)
    , Member(..)
    , RoomTask(..)
    , Message
    , File(..)
    , IncomingRequest(..)
    , IconPreset(..)
    , TaskStatus
    , AccountId

    , RoomIdWrap(..)
    , MessageIdWrap(..)
    , TaskIdsWrap(..)
    ) where

import ChatWork.Utils (strLength)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Text (Text)
import GHC.Generics (Generic)

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

data TaskStatus = Open | Done

instance Show TaskStatus where
  show Open = "open"
  show Done = "done"

type AccountId = Int

newtype RoomIdWrap = RoomIdWrap { getRoomId :: Int } deriving (Show, Generic)

instance ToJSON RoomIdWrap where
  toJSON = genericToJSON $ aesonDrop (strLength "get") snakeCase
instance FromJSON RoomIdWrap where
  parseJSON = genericParseJSON $ aesonDrop (strLength "get") snakeCase

newtype MessageIdWrap = MessageIdWrap { getMessageId :: Text } deriving (Show, Generic)

instance ToJSON MessageIdWrap where
  toJSON = genericToJSON $ aesonDrop (strLength "get") snakeCase
instance FromJSON MessageIdWrap where
  parseJSON = genericParseJSON $ aesonDrop (strLength "get") snakeCase

newtype TaskIdsWrap = TaskIdsWrap { getTaskIds :: [Int] } deriving (Show, Generic)

instance ToJSON TaskIdsWrap where
  toJSON = genericToJSON $ aesonDrop (strLength "get") snakeCase
instance FromJSON TaskIdsWrap where
  parseJSON = genericParseJSON $ aesonDrop (strLength "get") snakeCase
