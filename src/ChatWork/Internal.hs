module ChatWork.Internal
    ( req
    ) where

import ChatWork.Endpoints (getHttpResponse')

import Data.Proxy (Proxy)
import Network.HTTP.Req ( MonadHttp, HttpMethod, HttpBody, HttpResponse, Url
                        , HttpBodyAllowed, AllowsBody, ProvidesBody, Option, req')

req ::
 ( MonadHttp m, HttpMethod method, HttpBody body, HttpResponse response
 , HttpBodyAllowed (AllowsBody method) (ProvidesBody body))
  => method -> Url scheme -> body -> Proxy response -> Option scheme -> m response
req method url body = flip (req' method url body) . getHttpResponse'
