{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Rooms
    ( getRooms
    , postRoom
    , getRoom
    ) where

import Data.Monoid ((<>))
import ChatWork.Endpoints
import ChatWork.Types
import Network.HTTP.Req ( MonadHttp, JsonResponse, NoReqBody(..), ReqBodyUrlEnc(..)
                        , GET(..), POST(..)
                        , (/:), (/~), (=:), jsonResponse)
import ChatWork.Internal (req)

getRooms :: (MonadHttp m) => Token -> m (JsonResponse GetRoomsResponse)
getRooms = req GET (baseUrl /: "rooms") NoReqBody jsonResponse . mkTokenHeader

postRoom :: (MonadHttp m) => Token -> CreateRoomParams -> m (JsonResponse PostRoomResponse)
postRoom t params = req POST (baseUrl /: "rooms") (ReqBodyUrlEnc params') jsonResponse $ mkTokenHeader t
  where
    params' = maybe mempty ("description" =:) (cRoomDescription params)
           <> maybe mempty ("icon_preset" =:) (show <$> cIconPreset params)
           <> "members_admin_ids" =: (show =<< cMembersAdminIds params)
           <> maybe mempty ("members_member_ids" =:) ((show =<<) <$> cMembersMemberIds params)
           <> maybe mempty ("members_readonly_ids" =:) ((show =<<) <$> cMembersReadonlyIds params)
           <> "name" =: (cRoomName params)

getRoom :: (MonadHttp m) => Token -> Int -> m (JsonResponse GetRoomResponse)
getRoom t n = req GET (baseUrl /: "rooms" /~ n) NoReqBody jsonResponse $ mkTokenHeader t
