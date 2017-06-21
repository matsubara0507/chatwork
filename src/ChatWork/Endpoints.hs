{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Endpoints
    ( baseUrl
    , mkTokenHeader
    , getHttpResponse'
    , fixEmptyStringManager
    , fixEmptyString
    ) where

import Data.Default.Class (def)
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Control.Monad.IO.Class (MonadIO(..))
import Network.Connection (initConnectionContext)
import Network.HTTP.Client ( Request, Manager, Response(..), BodyReader
                           , ManagerSettings(..), newManager)
import Network.HTTP.Client.Internal (constBodyReader)
import Network.HTTP.Client.TLS (mkManagerSettingsContext)
import Network.HTTP.Req ( HttpResponse(..), Url, Scheme(Https), Option
                        , https, (/:), header, MonadHttp)
import Network.HTTP.Types.Header (hContentLength)
import ChatWork.Types

baseUrl :: Url 'Https
baseUrl = https "api.chatwork.com" /: "v2"

mkTokenHeader :: Token -> Option 'Https
mkTokenHeader token = header "X-ChatWorkToken" token

getHttpResponse' :: (HttpResponse a, MonadHttp m) => Proxy a -> Request -> Manager -> m a
getHttpResponse' Proxy r m = liftIO $ getHttpResponse r =<< fixEmptyStringManager

fixEmptyStringManager :: IO Manager
fixEmptyStringManager = do
  context <- initConnectionContext
  let settings = mkManagerSettingsContext (Just context) def Nothing
  newManager $ settings { managerModifyResponse = fixEmptyString }

fixEmptyString :: Response BodyReader -> IO (Response BodyReader)
fixEmptyString res = do
  reader <- constBodyReader ["[]"]
  let
    contentLength = fromMaybe "0" $ lookup hContentLength (responseHeaders res)
  return $ if contentLength /= "0" then res else res { responseBody = reader }
