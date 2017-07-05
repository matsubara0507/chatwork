-- |
-- see : <http://developer.chatwork.com/ja/endpoint_me.html>
--
{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Me
    ( getMe
    ) where

import           ChatWork.Internal (req)
import           ChatWork.Types    (ChatWorkResponse, Me)
import           ChatWork.Utils    (Token, baseUrl, mkTokenHeader)
import           Network.HTTP.Req  (GET (..), MonadHttp, NoReqBody (..),
                                    jsonResponse, (/:))

getMe :: (MonadHttp m) => Token -> m (ChatWorkResponse Me)
getMe = req GET (baseUrl /: "me") NoReqBody jsonResponse . mkTokenHeader
