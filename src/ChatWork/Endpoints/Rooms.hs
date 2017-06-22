{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Rooms
    ( getRooms
    , postRoom
    , getRoom
    , putRoom
    , deleteRoom
    , getRoomMembers
    , putRoomMembers
    ) where

import Data.Monoid ((<>))
import Network.HTTP.Req ( MonadHttp, JsonResponse, NoReqBody(..), ReqBodyUrlEnc(..)
                        , IgnoreResponse, GET(..), POST(..), PUT(..), DELETE(..)
                        , (/:), (/~), (=:), jsonResponse, ignoreResponse)
import ChatWork.Endpoints (baseUrl, mkTokenHeader, DELETE2(..))
import ChatWork.Internal (req)
import ChatWork.Types ( Token, GetRoomsResponse, PostRoomResponse
                      , GetRoomResponse, CreateRoomParams(..), ToReqParam(..)
                      , UpdateRoomParams(..), PutRoomResponse, DeleteRoomActionType
                      , GetRoomMembersResponse, PutRoomMembersResponse, RoomMembersParams(..))

getRooms :: (MonadHttp m) => Token -> m (JsonResponse GetRoomsResponse)
getRooms = req GET (baseUrl /: "rooms") NoReqBody jsonResponse . mkTokenHeader

postRoom :: (MonadHttp m) => Token -> CreateRoomParams -> m (JsonResponse PostRoomResponse)
postRoom t params = req POST (baseUrl /: "rooms") (ReqBodyUrlEnc params') jsonResponse $ mkTokenHeader t
  where
    params' = toReqParam "description" (cRoomDescription params)
           <> toReqParam "icon_preset" (cIconPreset params)
           <> toReqParam "members_admin_ids" (cMembersAdminIds params)
           <> toReqParam "members_member_ids" (cMembersMemberIds params)
           <> toReqParam "members_readonly_ids" (cMembersReadonlyIds params)
           <> toReqParam "name" (cRoomName params)

getRoom :: (MonadHttp m) => Token -> Int -> m (JsonResponse GetRoomResponse)
getRoom t n = req GET (baseUrl /: "rooms" /~ n) NoReqBody jsonResponse $ mkTokenHeader t

putRoom :: (MonadHttp m) => Token -> Int -> UpdateRoomParams -> m (JsonResponse PutRoomResponse)
putRoom t n params = req PUT (baseUrl /: "rooms" /~ n) (ReqBodyUrlEnc params') jsonResponse $ mkTokenHeader t
  where
    params' = toReqParam "description" (uRoomDescription params)
           <> toReqParam "icon_preset" (uIconPreset params)
           <> toReqParam "name" (uRoomName params)

deleteRoom :: (MonadHttp m) => Token -> Int -> DeleteRoomActionType -> m IgnoreResponse
deleteRoom t n action = req DELETE2 (baseUrl /: "rooms" /~ n) (ReqBodyUrlEnc params') ignoreResponse $ mkTokenHeader t
  where
    params' = "action_type" =: show action

getRoomMembers :: (MonadHttp m) => Token -> Int -> m (JsonResponse GetRoomMembersResponse)
getRoomMembers t n = req GET (baseUrl /: "rooms" /~ n /: "members") NoReqBody jsonResponse $ mkTokenHeader t

putRoomMembers :: (MonadHttp m) => Token -> Int -> RoomMembersParams -> m (JsonResponse PutRoomMembersResponse)
putRoomMembers t n params = req PUT (baseUrl /: "rooms" /~ n /: "members") (ReqBodyUrlEnc params') jsonResponse $ mkTokenHeader t
  where
    params' = toReqParam "members_admin_ids" (getAdminIds params)
           <> toReqParam "members_member_ids" (getMemberIds params)
           <> toReqParam "members_readonly_ids" (getReadonlyIds params)
