-- |
-- see : <http://developer.chatwork.com/ja/endpoint_my.html>

{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.My
    ( getMyStatus
    , getMyTasks
    ) where

import           ChatWork.Client   (Client (..))
import           ChatWork.Internal (req)
import           ChatWork.Types    (ChatWorkResponse, MyStatus, MyTasks)
import           ChatWork.Utils    (mkTokenHeader)
import           Network.HTTP.Req  (GET (..), MonadHttp, NoReqBody (..),
                                    jsonResponse, (/:))

getMyStatus :: (MonadHttp m, Client c) => c -> m (ChatWorkResponse MyStatus)
getMyStatus c = req GET (baseUrl c /: "my" /: "status") NoReqBody jsonResponse . mkTokenHeader $ token c

getMyTasks :: (MonadHttp m, Client c) => c -> m (ChatWorkResponse MyTasks)
getMyTasks c = req GET (baseUrl c /: "my" /: "tasks") NoReqBody jsonResponse . mkTokenHeader $ token c
