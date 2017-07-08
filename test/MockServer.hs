{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module MockServer
    ( mockServer
    ) where

import           ChatWork.Test.Types
import           ChatWork.Test.Values
import           ChatWork.Types
import           Data.Aeson
import qualified Data.ByteString.Lazy     as LBS (length)
import           Data.Int                 (Int64)
import           Network.Wai.Handler.Warp
import           Servant

type ChatWorkHeader a = Headers '[Header "Content-Length" Int64] a

type API = "me" :> Get '[JSON] (ChatWorkHeader Me)
      :<|> "my" :> "status" :> Get '[JSON] (ChatWorkHeader MyStatus)
      :<|> "my" :> "tasks"
             :> QueryParam "assigned_by_account_id" (Maybe AccountId)
             :> QueryParam "status" (Maybe TaskStatus)
             :> Get '[JSON] (ChatWorkHeader MyTasks)
      :<|> "contacts" :> Get '[JSON] (ChatWorkHeader Contacts)
      :<|> "incoming_requests" :> Get '[JSON] (ChatWorkHeader IncomingRequests)
      :<|> "incoming_requests" :> Capture "request_id" Int
             :> Put '[JSON] (ChatWorkHeader AcceptedIncomingRequest)
      :<|> "incoming_requests" :> Capture "request_id" Int
             :> Delete '[JSON] (ChatWorkHeader ())
      :<|> "rooms" :> Get '[JSON] (ChatWorkHeader Rooms)
      :<|> "rooms" :> ReqBody '[FormUrlEncoded] CreateRoomParams
             :> Post '[JSON] (ChatWorkHeader RoomIdWrap)
      :<|> "rooms" :> Capture "room_id" Int
             :> Get '[JSON] (ChatWorkHeader RoomDetail)
      :<|> "rooms" :> Capture "room_id" Int
             :> ReqBody '[FormUrlEncoded] UpdateRoomParams
             :> Put '[JSON] (ChatWorkHeader RoomIdWrap)
      :<|> "rooms" :> Capture "room_id" Int
             :> ReqBody '[FormUrlEncoded] DeleteRoomActionType
             :> Delete '[JSON] (ChatWorkHeader ())
      :<|> "rooms" :> Capture "room_id" Int :> "members"
             :> Get '[JSON] (ChatWorkHeader Members)
      :<|> "rooms" :> Capture "room_id" Int :> "members"
             :> ReqBody '[FormUrlEncoded] RoomMembersParams
             :> Put '[JSON] (ChatWorkHeader MembersPermission)
      :<|> "rooms" :> Capture "room_id" Int :> "messages"
             :> QueryParam "force" (Maybe Bool)
             :> Get '[JSON] (ChatWorkHeader Messages)
      :<|> "rooms" :> Capture "room_id" Int :> "messages"
             :> ReqBody '[FormUrlEncoded] MessageParams
             :> Post '[JSON] (ChatWorkHeader MessageIdWrap)
      :<|> "rooms" :> Capture "room_id" Int :> "messages"
             :> Capture "message_id" Int
             :> Get '[JSON] (ChatWorkHeader Message)
      :<|> "rooms" :> Capture "room_id" Int :> "tasks"
             :> QueryParam "account_id" (Maybe AccountId)
             :> QueryParam "assigned_by_account_id" (Maybe AccountId)
             :> QueryParam "status" (Maybe TaskStatus)
             :> Get '[JSON] (ChatWorkHeader RoomTasks)
      :<|> "rooms" :> Capture "room_id" Int :> "tasks"
             :> ReqBody '[FormUrlEncoded] CreateTaskParams
             :> Post '[JSON] (ChatWorkHeader TaskIdsWrap)
      :<|> "rooms" :> Capture "room_id" Int :> "tasks"
             :> Capture "task_id" Int
             :> Get '[JSON] (ChatWorkHeader RoomTask)
      :<|> "rooms" :> Capture "room_id" Int :> "files"
             :> QueryParam "account_id" (Maybe AccountId)
             :> Get '[JSON] (ChatWorkHeader Files)
      :<|> "rooms" :> Capture "room_id" Int :> "files"
             :> Capture "file_id" Int
             :> QueryParam "create_download_url" (Maybe Bool)
             :> Get '[JSON] (ChatWorkHeader File)

api :: Proxy API
api = Proxy

server :: Server API
server = getMe :<|> getMyStatus :<|> getMyTasks :<|> getContacts
    :<|> getIncomingRequests :<|> acceptIncomingRequest :<|> rejectIncomingRequest
    :<|> getRooms :<|> createRoom :<|> getRoom :<|> updateRoom :<|> deleteRoom
    :<|> getMembers :<|> updateMembersPermission
    :<|> getMessages :<|> postMessage :<|> getMessage
    :<|> getRoomTasks :<|> createTask :<|> getRoomTask :<|> getFiles :<|> getFile
  where
    getMe = return $ addHeader (LBS.length $ encode me) me
    getMyStatus = return $ addHeader (LBS.length $ encode myStatus) myStatus
    getMyTasks _ _ = return $ addHeader (LBS.length $ encode myTasks) myTasks
    getContacts = return $ addHeader (LBS.length $ encode contacts) contacts
    getIncomingRequests = return $ addHeader (LBS.length $ encode incomingRequests) incomingRequests
    acceptIncomingRequest _ = return $ addHeader (LBS.length $ encode acceptedIncomingRequest) acceptedIncomingRequest
    rejectIncomingRequest _ = return $ addHeader (LBS.length $ encode ()) ()
    getRooms = return $ addHeader (LBS.length $ encode rooms) rooms
    createRoom _ = return $ addHeader (LBS.length $ encode roomId) roomId
    getRoom _ = return $ addHeader (LBS.length $ encode roomDetail) roomDetail
    updateRoom _ _ = return $ addHeader (LBS.length $ encode roomId) roomId
    deleteRoom _ _ = return $ addHeader (LBS.length $ encode ()) ()
    getMembers _ = return $ addHeader (LBS.length $ encode members) members
    updateMembersPermission _ _ = return $ addHeader (LBS.length $ encode membersPermission) membersPermission
    getMessages _ _ = return $ addHeader (LBS.length $ encode messages) messages
    postMessage _ _ = return $ addHeader (LBS.length $ encode messageId) messageId
    getMessage _ _ = return $ addHeader (LBS.length $ encode message) message
    getRoomTasks _ _ _ _ = return $ addHeader (LBS.length $ encode roomTasks) roomTasks
    createTask _ _ = return $ addHeader (LBS.length $ encode taskIds) taskIds
    getRoomTask _ _ = return $ addHeader (LBS.length $ encode roomTask) roomTask
    getFiles _ _ = return $ addHeader (LBS.length $ encode files) files
    getFile _ _ _ = return $ addHeader (LBS.length $ encode file) file

mockServer :: IO ()
mockServer = run 8000 (serve api server)
