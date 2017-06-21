{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.My
    ( getMyStatus
    , getMyTasks
    ) where

import Network.HTTP.Req ( MonadHttp, JsonResponse, NoReqBody(..), GET(..)
                        , (/:), jsonResponse)
import ChatWork.Endpoints (baseUrl, mkTokenHeader)
import ChatWork.Internal (req)
import ChatWork.Types (Token, GetMyStatusResponse, GetMyTasksResponse)

getMyStatus :: (MonadHttp m) => Token -> m (JsonResponse GetMyStatusResponse)
getMyStatus = req GET (baseUrl /: "my" /: "status") NoReqBody jsonResponse . mkTokenHeader

getMyTasks :: (MonadHttp m) => Token -> m (JsonResponse GetMyTasksResponse)
getMyTasks = req GET (baseUrl /: "my" /: "tasks") NoReqBody jsonResponse . mkTokenHeader
