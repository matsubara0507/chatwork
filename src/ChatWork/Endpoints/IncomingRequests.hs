-- |
-- see : <http://developer.chatwork.com/ja/endpoint_incoming_requests.html>

{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.IncomingRequests
    ( getIncomingRequests
    , acceptIncomingRequests
    , rejectIncomingRequests
    ) where

import           ChatWork.Client   (Client (..))
import           ChatWork.Internal (req)
import           ChatWork.Types    (AcceptedIncomingRequest, ChatWorkResponse,
                                    IncomingRequests)
import           ChatWork.Utils    (mkTokenHeader)
import           Network.HTTP.Req  (DELETE (..), GET (..), MonadHttp,
                                    NoReqBody (..), PUT (..), jsonResponse,
                                    (/:), (/~))

getIncomingRequests :: (MonadHttp m, Client c) => c -> m (ChatWorkResponse IncomingRequests)
getIncomingRequests c = req GET (baseUrl c /: "incoming_requests") NoReqBody jsonResponse . mkTokenHeader $ token c

-- |
-- argument 'Int' is `request_id`
acceptIncomingRequests :: (MonadHttp m, Client c) => c -> Int -> m (ChatWorkResponse AcceptedIncomingRequest)
acceptIncomingRequests c n = req PUT (baseUrl c /: "incoming_requests" /~ n ) NoReqBody jsonResponse . mkTokenHeader $ token c

-- |
-- argument 'Int' is `request_id`
rejectIncomingRequests :: (MonadHttp m, Client c) => c -> Int -> m (ChatWorkResponse ())
rejectIncomingRequests c n = req DELETE (baseUrl c /: "incoming_requests" /~ n) NoReqBody jsonResponse . mkTokenHeader $ token c
