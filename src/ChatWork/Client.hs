{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Client (
      Token
    , ChatWorkClient(..)
    , Client(..)
    ) where

import           Data.ByteString  (ByteString)
import           Network.HTTP.Req (Scheme (Https), Url, https, (/:))


type Token = ByteString

newtype ChatWorkClient = ChatWorkClient Token

class Client a where
  baseUrl :: a -> Url 'Https
  token :: a -> Token

instance Client ChatWorkClient where
  baseUrl = const (https "api.chatwork.com" /: "v2")
  token (ChatWorkClient t) = t
