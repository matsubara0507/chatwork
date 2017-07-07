-- |
-- see : <http://developer.chatwork.com/ja/endpoint_me.html>
--
{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Me
    ( getMe
    ) where

import           ChatWork.Client   (Client (..))
import           ChatWork.Internal (req)
import           ChatWork.Types    (ChatWorkResponse, Me)
import           Network.HTTP.Req  (GET (..), MonadHttp, NoReqBody (..),
                                    jsonResponse, (/:))

getMe :: (MonadHttp m, Client c) => c -> m (ChatWorkResponse Me)
getMe c = req GET (baseUrl c /: "me") NoReqBody jsonResponse $ mkHeader c
