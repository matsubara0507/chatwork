{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.My
    ( getMyStatus
    , getMyTasks
    ) where

import Network.HTTP.Req ( MonadHttp, JsonResponse, NoReqBody(..), GET(..)
                        , (/:), jsonResponse)
import ChatWork.Utils (Token, baseUrl, mkTokenHeader)
import ChatWork.Internal (req)
import ChatWork.Types (ChatWorkResponse, MyStatus, MyTasks)

getMyStatus :: (MonadHttp m) => Token -> m (JsonResponse (ChatWorkResponse MyStatus))
getMyStatus = req GET (baseUrl /: "my" /: "status") NoReqBody jsonResponse . mkTokenHeader

getMyTasks :: (MonadHttp m) => Token -> m (JsonResponse (ChatWorkResponse MyTasks))
getMyTasks = req GET (baseUrl /: "my" /: "tasks") NoReqBody jsonResponse . mkTokenHeader
