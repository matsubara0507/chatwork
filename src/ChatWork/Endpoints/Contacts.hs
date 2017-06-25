{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Contacts
    ( getContacts
    ) where

import Network.HTTP.Req ( MonadHttp, JsonResponse, NoReqBody(..), GET(..)
                        , (/:), jsonResponse)
import ChatWork.Utils (Token, baseUrl, mkTokenHeader)
import ChatWork.Internal (req)
import ChatWork.Types (GetContactsResponse)

getContacts :: (MonadHttp m) => Token -> m (JsonResponse GetContactsResponse)
getContacts = req GET (baseUrl /: "contacts") NoReqBody jsonResponse . mkTokenHeader
