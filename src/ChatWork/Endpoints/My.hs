-- |
-- see : <http://developer.chatwork.com/ja/endpoint_my.html>

{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.My
    ( getMyStatus
    , getMyTasks
    ) where

import           ChatWork.Internal (req)
import           ChatWork.Types    (ChatWorkResponse, MyStatus, MyTasks)
import           ChatWork.Utils    (Token, baseUrl, mkTokenHeader)
import           Network.HTTP.Req  (GET (..), MonadHttp, NoReqBody (..),
                                    jsonResponse, (/:))

getMyStatus :: (MonadHttp m) => Token -> m (ChatWorkResponse MyStatus)
getMyStatus = req GET (baseUrl /: "my" /: "status") NoReqBody jsonResponse . mkTokenHeader

getMyTasks :: (MonadHttp m) => Token -> m (ChatWorkResponse MyTasks)
getMyTasks = req GET (baseUrl /: "my" /: "tasks") NoReqBody jsonResponse . mkTokenHeader
