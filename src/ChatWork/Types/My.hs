{-# LANGUAGE DeriveGeneric #-}

module ChatWork.Types.My
    ( MyStatus(..)
    , MyTasks
    , MyTask(..)
    , GetMyTasksParams(..)
    ) where

import           ChatWork.Types.Base (Account, AccountId, Room, TaskStatus)
import           ChatWork.Utils      (strLength)
import           Data.Aeson          (FromJSON (..), ToJSON (..),
                                      genericParseJSON, genericToJSON)
import           Data.Aeson.Casing   (aesonDrop, snakeCase)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)

data MyStatus = MyStatus
              { myStatusToUnreadRoomNum  :: Int
              , myStatusToMentionRoomNum :: Int
              , myStatusToMytaskRoomNum  :: Int
              , myStatusToUnreadNum      :: Int
              , myStatusToMentionNum     :: Int
              , myStatusToMytaskNum      :: Int
              } deriving (Eq, Show, Generic)

instance ToJSON MyStatus where
  toJSON = genericToJSON $ aesonDrop (strLength "myStatusTo") snakeCase
instance FromJSON MyStatus where
  parseJSON = genericParseJSON $ aesonDrop (strLength "myStatusTo") snakeCase

type MyTasks = [MyTask]

data MyTask = MyTask
          { myTaskToTaskId            :: Int
          , myTaskToRoom              :: Room
          , myTaskToAssignedByAccount :: Account
          , myTaskToMessageId         :: Text
          , myTaskToBody              :: Text
          , myTaskToLimitTime         :: Int
          , myTaskToStatus            :: Text
          } deriving (Eq, Show, Generic)

instance ToJSON MyTask where
  toJSON = genericToJSON $ aesonDrop (strLength "myTaskTo") snakeCase
instance FromJSON MyTask where
  parseJSON = genericParseJSON $ aesonDrop (strLength "myTaskTo") snakeCase

-- |
-- see: <http://developer.chatwork.com/ja/endpoint_rooms.html#GET-rooms-room_id-tasks>
data GetMyTasksParams = GetMyTasksParams
                   { getMyTasksAssignedByAccountId :: Maybe AccountId
                   , getMyTasksStatus              :: Maybe TaskStatus
                   } deriving (Show)
