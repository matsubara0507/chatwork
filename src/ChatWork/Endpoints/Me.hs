{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Me
    ( getMe
    ) where

import Network.HTTP.Req ( MonadHttp, JsonResponse, NoReqBody(..), GET(..)
                        , (/:), jsonResponse)
import ChatWork.Endpoints (baseUrl, mkTokenHeader)
import ChatWork.Internal (req)
import ChatWork.Types (Token, GetMeResponse)

getMe :: (MonadHttp m) => Token -> m (JsonResponse GetMeResponse)
getMe = req GET (baseUrl /: "me") NoReqBody jsonResponse . mkTokenHeader
