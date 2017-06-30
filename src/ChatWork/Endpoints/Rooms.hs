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

import           ChatWork.Internal (req)
import           ChatWork.Types    (AccountId, ChatWorkResponse,
                                    CreateRoomParams (..),
                                    CreateTaskParams (..), CreateUrlFlag,
                                    DeleteRoomActionType (..), File, Files,
                                    Force, GetTasksParams (..), Members,
                                    MembersPermission, Message, MessageBody,
                                    MessageIdWrap, Messages, RoomDetail,
                                    RoomIdWrap, RoomMembersParams (..),
                                    RoomTask, RoomTasks, Rooms, TaskIdsWrap,
                                    ToReqParam (..), UpdateRoomParams (..))
import           ChatWork.Utils    (DELETE2 (..), Token, baseUrl, mkTokenHeader)
import           Data.Bool         (bool)
import           Data.Monoid       ((<>))
import           Data.Text         (Text)
import           Network.HTTP.Req  (DELETE (..), GET (..), JsonResponse,
                                    MonadHttp, NoReqBody (..), POST (..),
                                    PUT (..), ReqBodyUrlEnc (..), jsonResponse,
                                    (/:), (/~), (=:))

getRooms :: (MonadHttp m) => Token -> m (ChatWorkResponse Rooms)
getRooms = req GET (baseUrl /: "rooms") NoReqBody jsonResponse . mkTokenHeader

createRoom :: (MonadHttp m) => Token -> CreateRoomParams -> m (ChatWorkResponse RoomIdWrap)
createRoom t params = req POST (baseUrl /: "rooms") (ReqBodyUrlEnc params') jsonResponse $ mkTokenHeader t
  where
    params' = toReqParam "description" (cRoomDescription params)
           <> toReqParam "icon_preset" (cIconPreset params)
           <> toReqParam "members_admin_ids" (cMembersAdminIds params)
           <> toReqParam "members_member_ids" (cMembersMemberIds params)
           <> toReqParam "members_readonly_ids" (cMembersReadonlyIds params)
           <> toReqParam "name" (cRoomName params)

getRoom :: (MonadHttp m) => Token -> Int -> m (ChatWorkResponse RoomDetail)
getRoom t n = req GET (baseUrl /: "rooms" /~ n) NoReqBody jsonResponse $ mkTokenHeader t

updateRoom :: (MonadHttp m) => Token -> Int -> UpdateRoomParams -> m (ChatWorkResponse RoomIdWrap)
updateRoom t n params = req PUT (baseUrl /: "rooms" /~ n) (ReqBodyUrlEnc params') jsonResponse $ mkTokenHeader t
  where
    params' = toReqParam "description" (uRoomDescription params)
           <> toReqParam "icon_preset" (uIconPreset params)
           <> toReqParam "name" (uRoomName params)

deleteRoom :: (MonadHttp m) => Token -> Int -> m (ChatWorkResponse ())
deleteRoom t n = deleteRoom' t n DeleteRoom

leaveRoom :: (MonadHttp m) => Token -> Int -> m (ChatWorkResponse ())
leaveRoom t n = deleteRoom' t n LeaveRoom

deleteRoom' :: (MonadHttp m) => Token -> Int -> DeleteRoomActionType -> m (ChatWorkResponse ())
deleteRoom' t n action = req DELETE2 (baseUrl /: "rooms" /~ n) (ReqBodyUrlEnc params') jsonResponse $ mkTokenHeader t
  where
    params' = "action_type" =: show action

getMembers :: (MonadHttp m) => Token -> Int -> m (ChatWorkResponse Members)
getMembers t n = req GET (baseUrl /: "rooms" /~ n /: "members") NoReqBody jsonResponse $ mkTokenHeader t

updateMembersPermission :: (MonadHttp m) => Token -> Int -> RoomMembersParams -> m (ChatWorkResponse MembersPermission)
updateMembersPermission t n params = req PUT (baseUrl /: "rooms" /~ n /: "members") (ReqBodyUrlEnc params') jsonResponse $ mkTokenHeader t
  where
    params' = toReqParam "members_admin_ids" (getAdminIds params)
           <> toReqParam "members_member_ids" (getMemberIds params)
           <> toReqParam "members_readonly_ids" (getReadonlyIds params)

getMessages :: (MonadHttp m) => Token -> Int -> Maybe Force -> m (ChatWorkResponse Messages)
getMessages t n force = req GET (baseUrl /: "rooms" /~ n /: "messages") NoReqBody jsonResponse $ mkTokenHeader t <> params'
  where
    params' = toReqParam "force" (bool 0 (1 :: Int) <$> force)

postMessage :: (MonadHttp m) => Token -> Int -> MessageBody -> m (ChatWorkResponse MessageIdWrap)
postMessage t n body = req POST (baseUrl /: "rooms" /~ n /: "messages") (ReqBodyUrlEnc params') jsonResponse $ mkTokenHeader t
  where
    params' = toReqParam "body" body

getMessage :: (MonadHttp m) => Token -> Int -> Text -> m (ChatWorkResponse Message)
getMessage t rid mid = req GET (baseUrl /: "rooms" /~ rid /: "messages" /~ mid) NoReqBody jsonResponse $ mkTokenHeader t

getRoomTasks :: (MonadHttp m) => Token -> Int -> GetTasksParams -> m (ChatWorkResponse RoomTasks)
getRoomTasks t n params = req GET (baseUrl /: "rooms" /~ n /: "tasks") NoReqBody jsonResponse $ mkTokenHeader t <> params'
  where
    params' = toReqParam "account_id" (getTaskAccountId params)
           <> toReqParam "assigned_by_account_id" (getTaskAssignedByAccountId params)
           <> toReqParam "status" (getTaskStatus params)

createTask :: (MonadHttp m) => Token -> Int -> CreateTaskParams -> m (ChatWorkResponse TaskIdsWrap)
createTask t n params = req POST (baseUrl /: "rooms" /~ n /: "tasks") (ReqBodyUrlEnc params') jsonResponse $ mkTokenHeader t
  where
    params' = toReqParam "body" (getTaskBody params)
           <> toReqParam "limit" (getTaskLimit params)
           <> toReqParam "to_ids" (getTaskToIds params)

getRoomTask :: (MonadHttp m) => Token -> Int -> Int -> m (ChatWorkResponse RoomTask)
getRoomTask t rid tid = req GET (baseUrl /: "rooms" /~ rid /: "tasks" /~ tid) NoReqBody jsonResponse $ mkTokenHeader t

getFiles :: (MonadHttp m) => Token -> Int -> Maybe AccountId -> m (ChatWorkResponse Files)
getFiles t rid aid = req GET (baseUrl /: "rooms" /~ rid /: "files") NoReqBody jsonResponse $ mkTokenHeader t <> params'
  where
    params' = toReqParam "account_id" aid

getFile :: (MonadHttp m) => Token -> Int -> Int -> Maybe CreateUrlFlag -> m (ChatWorkResponse File)
getFile t rid fid flag = req GET (baseUrl /: "rooms" /~ rid /: "files" /~ fid) NoReqBody jsonResponse $ mkTokenHeader t <> params'
  where
    params' = toReqParam "create_download_url" (bool 0 (1 :: Int) <$> flag)
