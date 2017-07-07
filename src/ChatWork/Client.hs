{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module ChatWork.Client (
      Token
    , ChatWorkClient(..)
    , Client(..)
    ) where

import           Data.ByteString  (ByteString)
import           Network.HTTP.Req (Option, Scheme (Https), Url, header, https,
                                   (/:))


type Token = ByteString

newtype ChatWorkClient = ChatWorkClient Token

-- |
-- By using type class, the same functions can be used for mock servers and local hosts.
class Client a where
  type ClientScheme a :: Scheme
  baseUrl :: a -> Url (ClientScheme a)
  mkHeader :: a -> Option scheme

instance Client ChatWorkClient where
  type ClientScheme ChatWorkClient = 'Https
  baseUrl = const (https "api.chatwork.com" /: "v2")
  mkHeader (ChatWorkClient token) = header "X-ChatWorkToken" token
