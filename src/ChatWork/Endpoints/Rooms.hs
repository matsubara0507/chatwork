-- |
-- see : <http://developer.chatwork.com/ja/endpoint_rooms.html>

{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Rooms (
    -- * Room
      getRooms
    , createRoom
    , getRoom
    , updateRoom
    , deleteRoom
    , leaveRoom
    , deleteRoom'
    -- * Room Member
    , getMembers
    , updateMembersPermission
    -- * Room Message
    , getMessages
    , postMessage
    , getMessage
    -- * Room Task
    , getRoomTasks
    , createTask
    , getRoomTask
    -- * Room File
    , getFiles
    , getFile
    ) where

import           ChatWork.Client  (Client (..))
import           ChatWork.Types   (AccountId, ChatWorkResponse,
                                   CreateRoomParams (..), CreateTaskParams (..),
                                   CreateUrlFlag, DeleteRoomActionType (..),
                                   File, Files, Force, GetTasksParams (..),
                                   Members, MembersPermission, Message,
                                   MessageBody, MessageIdWrap, Messages,
                                   RoomDetail, RoomIdWrap,
                                   RoomMembersParams (..), RoomTask, RoomTasks,
                                   Rooms, TaskIdsWrap, ToReqParam (..),
                                   UpdateRoomParams (..), jsonResponse')
import           ChatWork.Utils   (DELETE2 (..))
import           Data.Bool        (bool)
import           Data.Monoid      ((<>))
import           Data.Text        (Text)
import           Network.HTTP.Req (GET (..), MonadHttp, NoReqBody (..),
                                   POST (..), PUT (..), ReqBodyUrlEnc (..), req,
                                   (/:), (/~), (=:))

getRooms :: (MonadHttp m, Client c) => c -> m (ChatWorkResponse Rooms)
getRooms c = req GET (baseUrl c /: "rooms") NoReqBody jsonResponse' $ mkHeader c

createRoom :: (MonadHttp m, Client c) => c -> CreateRoomParams -> m (ChatWorkResponse RoomIdWrap)
createRoom c params = req POST (baseUrl c /: "rooms") (ReqBodyUrlEnc params') jsonResponse' $ mkHeader c
  where
    params' = toReqParam "description" (cRoomDescription params)
           <> toReqParam "icon_preset" (cIconPreset params)
           <> toReqParam "members_admin_ids" (cMembersAdminIds params)
           <> toReqParam "members_member_ids" (cMembersMemberIds params)
           <> toReqParam "members_readonly_ids" (cMembersReadonlyIds params)
           <> toReqParam "name" (cRoomName params)

-- |
-- argumrnt 'Int' is `room_id`.
getRoom :: (MonadHttp m, Client c) => c -> Int -> m (ChatWorkResponse RoomDetail)
getRoom c n = req GET (baseUrl c /: "rooms" /~ n) NoReqBody jsonResponse' $ mkHeader c

-- |
-- argumrnt 'Int' is `room_id`.
updateRoom :: (MonadHttp m, Client c) => c -> Int -> UpdateRoomParams -> m (ChatWorkResponse RoomIdWrap)
updateRoom c n params = req PUT (baseUrl c /: "rooms" /~ n) (ReqBodyUrlEnc params') jsonResponse' $ mkHeader c
  where
    params' = toReqParam "description" (uRoomDescription params)
           <> toReqParam "icon_preset" (uIconPreset params)
           <> toReqParam "name" (uRoomName params)

-- |
-- wrap `deleteRoom'` function, using 'DeleteRoom'.
-- argumrnt 'Int' is `room_id`.
deleteRoom :: (MonadHttp m, Client c) => c -> Int -> m (ChatWorkResponse ())
deleteRoom c n = deleteRoom' c n DeleteRoom

-- |
-- wrap `deleteRoom'` function, using 'LeaveRoom'.
-- argumrnt 'Int' is `room_id`.
leaveRoom :: (MonadHttp m, Client c) => c -> Int -> m (ChatWorkResponse ())
leaveRoom c n = deleteRoom' c n LeaveRoom

-- |
-- argumrnt 'Int' is `room_id`.
deleteRoom' :: (MonadHttp m, Client c) => c -> Int -> DeleteRoomActionType -> m (ChatWorkResponse ())
deleteRoom' c n action = req DELETE2 (baseUrl c /: "rooms" /~ n) (ReqBodyUrlEnc params') jsonResponse' $ mkHeader c
  where
    params' = "action_type" =: show action

-- |
-- argumrnt 'Int' is `room_id`.
getMembers :: (MonadHttp m, Client c) => c -> Int -> m (ChatWorkResponse Members)
getMembers c n = req GET (baseUrl c /: "rooms" /~ n /: "members") NoReqBody jsonResponse' $ mkHeader c

-- |
-- argumrnt 'Int' is `room_id`.
updateMembersPermission :: (MonadHttp m, Client c) => c -> Int -> RoomMembersParams -> m (ChatWorkResponse MembersPermission)
updateMembersPermission c n params = req PUT (baseUrl c /: "rooms" /~ n /: "members") (ReqBodyUrlEnc params') jsonResponse' $ mkHeader c
  where
    params' = toReqParam "members_admin_ids" (getAdminIds params)
           <> toReqParam "members_member_ids" (getMemberIds params)
           <> toReqParam "members_readonly_ids" (getReadonlyIds params)

-- |
-- argumrnt 'Int' is `room_id`.
getMessages :: (MonadHttp m, Client c) => c -> Int -> Maybe Force -> m (ChatWorkResponse Messages)
getMessages c n force = req GET (baseUrl c /: "rooms" /~ n /: "messages") NoReqBody jsonResponse' $ mkHeader c <> params'
  where
    params' = toReqParam "force" (bool 0 (1 :: Int) <$> force)

-- |
-- argumrnt 'Int' is `room_id`.
postMessage :: (MonadHttp m, Client c) => c -> Int -> MessageBody -> m (ChatWorkResponse MessageIdWrap)
postMessage c n body = req POST (baseUrl c /: "rooms" /~ n /: "messages") (ReqBodyUrlEnc params') jsonResponse' $ mkHeader c
  where
    params' = toReqParam "body" body

-- |
-- argumrnt 'Int' is `room_id`.
-- argumrnt 'Text' is `message_id`.
getMessage :: (MonadHttp m, Client c) => c -> Int -> Text -> m (ChatWorkResponse Message)
getMessage c rid mid = req GET (baseUrl c /: "rooms" /~ rid /: "messages" /~ mid) NoReqBody jsonResponse' $ mkHeader c

-- |
-- argumrnt 'Int' is `room_id`.
getRoomTasks :: (MonadHttp m, Client c) => c -> Int -> GetTasksParams -> m (ChatWorkResponse RoomTasks)
getRoomTasks c n params = req GET (baseUrl c /: "rooms" /~ n /: "tasks") NoReqBody jsonResponse' $ mkHeader c <> params'
  where
    params' = toReqParam "account_id" (getTaskAccountId params)
           <> toReqParam "assigned_by_account_id" (getTaskAssignedByAccountId params)
           <> toReqParam "status" (getTaskStatus params)

-- |
-- argumrnt 'Int' is `room_id`.
createTask :: (MonadHttp m, Client c) => c -> Int -> CreateTaskParams -> m (ChatWorkResponse TaskIdsWrap)
createTask c n params = req POST (baseUrl c /: "rooms" /~ n /: "tasks") (ReqBodyUrlEnc params') jsonResponse' $ mkHeader c
  where
    params' = toReqParam "body" (getTaskBody params)
           <> toReqParam "limit" (getTaskLimit params)
           <> toReqParam "to_ids" (getTaskToIds params)
-- |
-- argumrnt first 'Int' is `room_id`.
-- argumrnt second 'Int' is `task_id`.
getRoomTask :: (MonadHttp m, Client c) => c -> Int -> Int -> m (ChatWorkResponse RoomTask)
getRoomTask c rid tid = req GET (baseUrl c /: "rooms" /~ rid /: "tasks" /~ tid) NoReqBody jsonResponse' $ mkHeader c

-- |
-- argumrnt 'Int' is `room_id`.
getFiles :: (MonadHttp m, Client c) => c -> Int -> Maybe AccountId -> m (ChatWorkResponse Files)
getFiles c rid aid = req GET (baseUrl c /: "rooms" /~ rid /: "files") NoReqBody jsonResponse' $ mkHeader c <> params'
  where
    params' = toReqParam "account_id" aid

-- |
-- argumrnt first 'Int' is `room_id`.
-- argumrnt second 'Int' is `file_id`.
getFile :: (MonadHttp m, Client c) => c -> Int -> Int -> Maybe CreateUrlFlag -> m (ChatWorkResponse File)
getFile c rid fid flag = req GET (baseUrl c /: "rooms" /~ rid /: "files" /~ fid) NoReqBody jsonResponse' $ mkHeader c <> params'
  where
    params' = toReqParam "create_download_url" (bool 0 (1 :: Int) <$> flag)
