{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.IncomingRequests
    ( getIncomingRequests
    , putIncomingRequests
    , deleteIncomingRequests
    ) where

import Network.HTTP.Req ( MonadHttp, JsonResponse, NoReqBody(..), jsonResponse
                        , GET(..), PUT(..), DELETE(..), (/:), (/~))
import ChatWork.Utils (Token, baseUrl, mkTokenHeader)
import ChatWork.Internal (req)
import ChatWork.Types (ChatWorkResponse, IncomingRequests, AcceptedIncomingRequest)

getIncomingRequests :: (MonadHttp m) => Token -> m (ChatWorkResponse IncomingRequests)
getIncomingRequests = req GET (baseUrl /: "incoming_requests") NoReqBody jsonResponse . mkTokenHeader

putIncomingRequests :: (MonadHttp m) => Token -> Int -> m (ChatWorkResponse AcceptedIncomingRequest)
putIncomingRequests t n = req PUT (baseUrl /: "incoming_requests" /~ n ) NoReqBody jsonResponse $ mkTokenHeader t

deleteIncomingRequests :: (MonadHttp m) => Token -> Int -> m (ChatWorkResponse ())
deleteIncomingRequests t n = req DELETE (baseUrl /: "incoming_requests" /~ n) NoReqBody jsonResponse $ mkTokenHeader t
