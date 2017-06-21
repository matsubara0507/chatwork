{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.My
    ( getMyStatus
    , getMyTasks
    ) where

import ChatWork.Endpoints
import ChatWork.Types
import Network.HTTP.Req ( MonadHttp, JsonResponse, NoReqBody(..), GET(..)
                        , (/:), jsonResponse)
import ChatWork.Internal (req)

getMyStatus :: (MonadHttp m) => Token -> m (JsonResponse GetMyStatusResponse)
getMyStatus = req GET (baseUrl /: "my" /: "status") NoReqBody jsonResponse . mkTokenHeader

getMyTasks :: (MonadHttp m) => Token -> m (JsonResponse GetMyTasksResponse)
getMyTasks = req GET (baseUrl /: "my" /: "tasks") NoReqBody jsonResponse . mkTokenHeader
