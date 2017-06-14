{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.My
    ( getMyStatus
    , getMyTasks
    ) where

import Network.HTTP.Req
import ChatWork.Endpoints
import ChatWork.Types

getMyStatus :: (MonadHttp m) => Token -> m (JsonResponse GetMyStatusResponse)
getMyStatus = req GET (baseUrl /: "my" /: "status") NoReqBody jsonResponse . mkTokenHeader

getMyTasks :: (MonadHttp m) => Token -> m (JsonResponse GetMyTasksResponse)
getMyTasks = req GET (baseUrl /: "my" /: "tasks") NoReqBody jsonResponse . mkTokenHeader
