{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Test.Values.Rooms (
    -- * Room
      rooms
    , roomId
    , roomDetail
    -- * Room Member
    , members
    , membersPermission
    -- * Room Message
    , messages
    , messageId
    , message
    -- * Room Task
    , roomTasks
    , taskIds
    , roomTask
    -- * Room File
    , files
    , file
    -- * request params
    , createRoomParams
    , updateRoomParams
    , roomMembersParams
    , getTasksParams
    , createTaskParams
    ) where

import           ChatWork.Types (Account (..), CreateRoomParams (..),
                                 CreateTaskParams (..), File (..), Files,
                                 GetTasksParams (..), IconPreset (..),
                                 Member (..), Members, MembersPermission (..),
                                 Message (..), MessageIdWrap (..), Messages,
                                 RoomDetail (..), RoomIdWrap (..),
                                 RoomMembersParams (..), RoomTask (..),
                                 RoomTasks, Rooms, TaskIdsWrap (..),
                                 TaskStatus (..), UpdateRoomParams (..))

rooms :: Rooms
rooms = [ RoomDetail 123
                     "Group Chat Name"
                     "group"
                     "admin"
                     False
                     10
                     1
                     0
                     122
                     10
                     17
                     "https://example.com/ico_group.png"
                     1298905200
                     Nothing
        ]

roomId :: RoomIdWrap
roomId = RoomIdWrap 1234

roomDetail :: RoomDetail
roomDetail = RoomDetail 123
                        "Group Chat Name"
                        "group"
                        "admin"
                        False
                        10
                        1
                        0
                        122
                        10
                        17
                        "https://example.com/ico_group.png"
                        1298905200
                        (Just "room description text")
members :: Members
members = [ Member 123
                   "member"
                   "John Smith"
                   "tarochatworkid"
                   101
                   "Hello Company"
                   "Marketing"
                   "https://example.com/abc.png"
          ]

membersPermission :: MembersPermission
membersPermission = MembersPermission [123, 542, 1001]
                                      [10, 103]
                                      [6, 11]

messages :: Messages
messages = [ Message "5"
                     (Account 123 "Bob" "https://example.com/ico_avatar.png")
                     "Hello Chatwork!"
                     1384242850
                     0
           ]

messageId :: MessageIdWrap
messageId = MessageIdWrap "1234"

message :: Message
message = head messages

roomTasks :: RoomTasks
roomTasks = [ RoomTask 3
                       (Account 123 "Bob" "https://example.com/abc.png")
                       (Account 456 "Anna" "https://example.com/def.png")
                       "13"
                       "buy milk"
                       1384354799
                       "open"
            ]

taskIds :: TaskIdsWrap
taskIds = TaskIdsWrap [123, 124]

roomTask :: RoomTask
roomTask = head roomTasks

files :: Files
files = [ File 3
               (Account 123 "Bob" "https://example.com/ico_avatar.png")
               "22"
               "README.md"
               2232
               1384414750
        ]

file :: File
file = head files

createRoomParams :: CreateRoomParams
createRoomParams = CreateRoomParams (Just "group chat description")
                                    (Just Meeting)
                                    [123, 542, 1001]
                                    (Just [21, 344])
                                    (Just [15, 103])
                                    "Website renewal project"

updateRoomParams :: UpdateRoomParams
updateRoomParams = UpdateRoomParams (Just "group chat description")
                                    (Just Meeting)
                                    (Just "Website renewal project")

roomMembersParams :: RoomMembersParams
roomMembersParams = RoomMembersParams [123, 542, 1001]
                                      (Just [21, 344])
                                      (Just [15, 103])

getTasksParams :: GetTasksParams
getTasksParams = GetTasksParams (Just 101) (Just 78) (Just Done)

createTaskParams :: CreateTaskParams
createTaskParams = CreateTaskParams "Buy milk"
                                    (Just 1385996399)
                                    [1, 3, 6]
