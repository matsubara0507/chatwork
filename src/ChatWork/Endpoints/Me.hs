{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints.Me
    ( getMe
    ) where

import Network.HTTP.Req ( MonadHttp, JsonResponse, NoReqBody(..), GET(..)
                        , (/:), jsonResponse)
import ChatWork.Utils (Token, baseUrl, mkTokenHeader)
import ChatWork.Internal (req)
import ChatWork.Types (ChatWorkResponse, Me)

getMe :: (MonadHttp m) => Token -> m (ChatWorkResponse Me)
getMe = req GET (baseUrl /: "me") NoReqBody jsonResponse . mkTokenHeader
