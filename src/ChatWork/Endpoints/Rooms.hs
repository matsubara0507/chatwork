{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Rooms
    ( getRooms
    , postRoom
    , getRoom
    ) where

import Data.Monoid ((<>))
import Network.HTTP.Req ( MonadHttp, JsonResponse, NoReqBody(..), ReqBodyUrlEnc(..)
                        , GET(..), POST(..)
                        , (/:), (/~), (=:), jsonResponse)
import ChatWork.Endpoints (baseUrl, mkTokenHeader)
import ChatWork.Internal (req)
import ChatWork.Types ( Token, GetRoomsResponse, PostRoomResponse
                      , GetRoomResponse, CreateRoomParams(..), ToReqParam(..))

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
