{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Contacts
    ( getContacts
    ) where

import Network.HTTP.Req ( MonadHttp, JsonResponse, NoReqBody(..), GET(..)
                        , (/:), jsonResponse)
import ChatWork.Endpoints (baseUrl, mkTokenHeader)
import ChatWork.Internal (req)
import ChatWork.Types (Token, GetContactsResponse)

getContacts :: (MonadHttp m) => Token -> m (JsonResponse GetContactsResponse)
getContacts = req GET (baseUrl /: "contacts") NoReqBody jsonResponse . mkTokenHeader
