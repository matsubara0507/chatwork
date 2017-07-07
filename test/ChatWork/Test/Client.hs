{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}

module ChatWork.Test.Client (
      TestClient(..)
    ) where

import           ChatWork.Client (Client(..))
import           Network.HTTP.Req (http, Scheme(Http), port)


data TestClient = TestClient

instance Client TestClient where
  type ClientScheme TestClient = 'Http
  baseUrl = const (http "localhost")
  mkHeader = const (port 8000)
