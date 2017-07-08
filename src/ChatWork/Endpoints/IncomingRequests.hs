-- |
-- see : <http://developer.chatwork.com/ja/endpoint_incoming_requests.html>

{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.IncomingRequests
    ( getIncomingRequests
    , acceptIncomingRequest
    , rejectIncomingRequest
    ) where

import           ChatWork.Client   (Client (..))
import           ChatWork.Internal (req)
import           ChatWork.Types    (AcceptedIncomingRequest, ChatWorkResponse,
                                    IncomingRequests)
import           Network.HTTP.Req  (DELETE (..), GET (..), MonadHttp,
                                    NoReqBody (..), PUT (..), jsonResponse,
                                    (/:), (/~))

getIncomingRequests :: (MonadHttp m, Client c) => c -> m (ChatWorkResponse IncomingRequests)
getIncomingRequests c = req GET (baseUrl c /: "incoming_requests") NoReqBody jsonResponse $ mkHeader c

-- |
-- argument 'Int' is `request_id`
acceptIncomingRequest :: (MonadHttp m, Client c) => c -> Int -> m (ChatWorkResponse AcceptedIncomingRequest)
acceptIncomingRequest c n = req PUT (baseUrl c /: "incoming_requests" /~ n ) NoReqBody jsonResponse $ mkHeader c

-- |
-- argument 'Int' is `request_id`
rejectIncomingRequest :: (MonadHttp m, Client c) => c -> Int -> m (ChatWorkResponse ())
rejectIncomingRequest c n = req DELETE (baseUrl c /: "incoming_requests" /~ n) NoReqBody jsonResponse $ mkHeader c
