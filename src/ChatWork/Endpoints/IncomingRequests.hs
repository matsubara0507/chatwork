{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.IncomingRequests
    ( getIncomingRequests
    , putIncomingRequests
    , deleteIncomingRequests
    ) where

import Network.HTTP.Req
import ChatWork.Endpoints
import ChatWork.Types

getIncomingRequests :: (MonadHttp m) => Token -> m (JsonResponse GetIncomingRequestsResponse)
getIncomingRequests = flip (req' GET (baseUrl /: "incoming_requests") NoReqBody) (getHttpResponse' jsonResponse) . mkTokenHeader

putIncomingRequests :: (MonadHttp m) => Token -> Int -> m (JsonResponse PutIncomingRequestsResponse)
putIncomingRequests t n = req PUT (baseUrl /: "incoming_requests" /~ n ) NoReqBody jsonResponse $ mkTokenHeader t

deleteIncomingRequests :: (MonadHttp m) => Token -> Int -> m IgnoreResponse
deleteIncomingRequests t n = req DELETE (baseUrl /: "incoming_requests" /~ n) NoReqBody ignoreResponse $ mkTokenHeader t
