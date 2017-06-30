{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Contacts
    ( getContacts
    ) where

import Network.HTTP.Req ( MonadHttp, JsonResponse, NoReqBody(..), GET(..)
                        , (/:), jsonResponse)
import ChatWork.Utils (Token, baseUrl, mkTokenHeader)
import ChatWork.Internal (req)
import ChatWork.Types (ChatWorkResponse, Contacts)

getContacts :: (MonadHttp m) => Token -> m (JsonResponse (ChatWorkResponse Contacts))
getContacts = req GET (baseUrl /: "contacts") NoReqBody jsonResponse . mkTokenHeader
