{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints
    ( baseUrl
    , mkTokenHeader
    ) where

import Network.HTTP.Req
import ChatWork.Types

baseUrl :: Url 'Https
baseUrl = https "api.chatwork.com" /: "v2"

mkTokenHeader :: Token -> Option 'Https
mkTokenHeader token = header "X-ChatWorkToken" token
