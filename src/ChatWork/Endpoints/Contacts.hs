{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Contacts
    ( getContacts
    ) where

import Network.HTTP.Req
import ChatWork.Endpoints
import ChatWork.Types

getContacts :: (MonadHttp m) => Token -> m (JsonResponse GetContactsResponse)
getContacts = req GET (baseUrl /: "contacts") NoReqBody jsonResponse . mkTokenHeader
