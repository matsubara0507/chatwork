{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Me
    ( getMe
    ) where

import ChatWork.Endpoints
import ChatWork.Types
import Network.HTTP.Req ( MonadHttp, JsonResponse, NoReqBody(..), GET(..)
                        , (/:), jsonResponse)
import ChatWork.Internal (req)

getMe :: (MonadHttp m) => Token -> m (JsonResponse GetMeResponse)
getMe = req GET (baseUrl /: "me") NoReqBody jsonResponse . mkTokenHeader
