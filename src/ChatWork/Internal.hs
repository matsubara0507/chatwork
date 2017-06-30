{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ChatWork.Internal
    ( req
    ) where

import           ChatWork.Utils   (getHttpResponse')

import           Data.Proxy       (Proxy)
import           Network.HTTP.Req (AllowsBody, HttpBody, HttpBodyAllowed,
                                   HttpMethod, HttpResponse, MonadHttp, Option,
                                   ProvidesBody, Url, req')

-- |
-- Helper function for req'
-- getHttpResponse' is used my custamize Manager

req ::
  ( MonadHttp m, HttpMethod method, HttpBody body, HttpResponse response
  , HttpBodyAllowed (AllowsBody method) (ProvidesBody body))
  => method
  -> Url scheme
  -> body
  -> Proxy response
  -> Option scheme
  -> m response
req method url body = flip (req' method url body) . getHttpResponse'
