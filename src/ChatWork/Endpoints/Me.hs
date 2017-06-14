{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Me
    ( getMe
    ) where

import Network.HTTP.Req
import ChatWork.Endpoints
import ChatWork.Types

getMe :: (MonadHttp m) => Token -> m (JsonResponse GetMeResponse)
getMe = req GET (baseUrl /: "me") NoReqBody jsonResponse . mkTokenHeader
