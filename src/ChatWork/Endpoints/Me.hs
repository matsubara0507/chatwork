-- |
-- see : <http://developer.chatwork.com/ja/endpoint_me.html>
--
{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Me
    ( getMe
    ) where

import           ChatWork.Client  (Client (..))
import           ChatWork.Types   (ChatWorkResponse, Me, jsonResponse')
import           Network.HTTP.Req (GET (..), MonadHttp, NoReqBody (..), req,
                                   (/:))

getMe :: (MonadHttp m, Client c) => c -> m (ChatWorkResponse Me)
getMe c = req GET (baseUrl c /: "me") NoReqBody jsonResponse' $ mkHeader c
