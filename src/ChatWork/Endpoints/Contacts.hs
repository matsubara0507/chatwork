-- |
-- see : <http://developer.chatwork.com/ja/endpoint_contacts.html>

{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Contacts
    ( getContacts
    ) where

import           ChatWork.Internal (req)
import           ChatWork.Types    (ChatWorkResponse, Contacts)
import           ChatWork.Utils    (Token, baseUrl, mkTokenHeader)
import           Network.HTTP.Req  (GET (..), MonadHttp, NoReqBody (..),
                                    jsonResponse, (/:))

getContacts :: (MonadHttp m) => Token -> m (ChatWorkResponse Contacts)
getContacts = req GET (baseUrl /: "contacts") NoReqBody jsonResponse . mkTokenHeader
