-- |
-- see : <http://developer.chatwork.com/ja/endpoint_my.html>

{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.My
    ( getMyStatus
    , getMyTasks
    ) where

import           ChatWork.Client  (Client (..))
import           ChatWork.Types   (ChatWorkResponse, GetMyTasksParams (..),
                                   MyStatus, MyTasks, ToReqParam (..),
                                   jsonResponse')
import           Data.Monoid      ((<>))
import           Network.HTTP.Req (GET (..), MonadHttp, NoReqBody (..), req,
                                   (/:))

getMyStatus :: (MonadHttp m, Client c) => c -> m (ChatWorkResponse MyStatus)
getMyStatus c = req GET (baseUrl c /: "my" /: "status") NoReqBody jsonResponse' $ mkHeader c

getMyTasks :: (MonadHttp m, Client c) => c -> GetMyTasksParams -> m (ChatWorkResponse MyTasks)
getMyTasks c params = req GET (baseUrl c /: "my" /: "tasks") NoReqBody jsonResponse' $ mkHeader c <> params'
  where
    params' = toReqParam "assigned_by_account_id" (getMyTasksAssignedByAccountId params)
           <> toReqParam "status" (getMyTasksStatus params)
