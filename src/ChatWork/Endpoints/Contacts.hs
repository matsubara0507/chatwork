-- |
-- see : <http://developer.chatwork.com/ja/endpoint_contacts.html>

{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Contacts
    ( getContacts
    ) where

import           ChatWork.Client  (Client (..))
import           ChatWork.Types   (ChatWorkResponse, Contacts, jsonResponse')
import           Network.HTTP.Req (GET (..), MonadHttp, NoReqBody (..), req,
                                   (/:))

getContacts :: (MonadHttp m, Client c) => c -> m (ChatWorkResponse Contacts)
getContacts c = req GET (baseUrl c /: "contacts") NoReqBody jsonResponse' $ mkHeader c
