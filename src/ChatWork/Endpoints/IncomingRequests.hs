-- |
-- see : <http://developer.chatwork.com/ja/endpoint_incoming_requests.html>

{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.IncomingRequests
    ( getIncomingRequests
    , acceptIncomingRequests
    , rejectIncomingRequests
    ) where

import           ChatWork.Internal (req)
import           ChatWork.Types    (AcceptedIncomingRequest, ChatWorkResponse,
                                    IncomingRequests)
import           ChatWork.Utils    (Token, baseUrl, mkTokenHeader)
import           Network.HTTP.Req  (DELETE (..), GET (..), MonadHttp,
                                    NoReqBody (..), PUT (..), jsonResponse,
                                    (/:), (/~))

getIncomingRequests :: (MonadHttp m) => Token -> m (ChatWorkResponse IncomingRequests)
getIncomingRequests = req GET (baseUrl /: "incoming_requests") NoReqBody jsonResponse . mkTokenHeader

-- |
-- argument 'Int' is `request_id`
acceptIncomingRequests :: (MonadHttp m) => Token -> Int -> m (ChatWorkResponse AcceptedIncomingRequest)
acceptIncomingRequests t n = req PUT (baseUrl /: "incoming_requests" /~ n ) NoReqBody jsonResponse $ mkTokenHeader t

-- |
-- argument 'Int' is `request_id`
rejectIncomingRequests :: (MonadHttp m) => Token -> Int -> m (ChatWorkResponse ())
rejectIncomingRequests t n = req DELETE (baseUrl /: "incoming_requests" /~ n) NoReqBody jsonResponse $ mkTokenHeader t
