{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Me
    ( getMe
    ) where

import Network.HTTP.Req ( MonadHttp, JsonResponse, NoReqBody(..), GET(..)
                        , (/:), jsonResponse)
import ChatWork.Utils (Token, baseUrl, mkTokenHeader)
import ChatWork.Internal (req)
import ChatWork.Types (GetMeResponse)

getMe :: (MonadHttp m) => Token -> m (JsonResponse GetMeResponse)
getMe = req GET (baseUrl /: "me") NoReqBody jsonResponse . mkTokenHeader
