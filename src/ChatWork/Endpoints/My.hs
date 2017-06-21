{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.My
    ( getMyStatus
    , getMyTasks
    ) where

import Network.HTTP.Req
import ChatWork.Endpoints
import ChatWork.Types

getMyStatus :: (MonadHttp m) => Token -> m (JsonResponse GetMyStatusResponse)
getMyStatus = flip (req' GET (baseUrl /: "my" /: "status") NoReqBody) (getHttpResponse' jsonResponse) . mkTokenHeader

getMyTasks :: (MonadHttp m) => Token -> m (JsonResponse GetMyTasksResponse)
getMyTasks = flip (req' GET (baseUrl /: "my" /: "tasks") NoReqBody) (getHttpResponse' jsonResponse) . mkTokenHeader
