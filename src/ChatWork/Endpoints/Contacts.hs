{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Contacts
    ( getContacts
    ) where

import ChatWork.Endpoints
import ChatWork.Types
import Network.HTTP.Req ( MonadHttp, JsonResponse, NoReqBody(..), GET(..)
                        , (/:), jsonResponse)
import ChatWork.Internal (req)

getContacts :: (MonadHttp m) => Token -> m (JsonResponse GetContactsResponse)
getContacts = req GET (baseUrl /: "contacts") NoReqBody jsonResponse . mkTokenHeader
