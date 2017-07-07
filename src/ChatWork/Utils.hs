{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module ChatWork.Utils (
    -- * Custamize Managaer
      getHttpResponse'
    , fixEmptyStringManager
    , fixEmptyString
    -- * DELETE HTTP method with paramater
    , DELETE2(..)
    -- * help to make 'FromJSON' instance
    , strLength
    ) where

import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Default.Class           (def)
import           Data.List                    (lookup)
import           Data.Maybe                   (fromMaybe)
import           Data.Proxy                   (Proxy (..))
import           Network.Connection           (initConnectionContext)
import           Network.HTTP.Client          (BodyReader, Manager,
                                               ManagerSettings (..), Request,
                                               Response (..), newManager)
import           Network.HTTP.Client.Internal (constBodyReader)
import           Network.HTTP.Client.TLS      (mkManagerSettingsContext)
import           Network.HTTP.Req             (AllowsBody (..),
                                               CanHaveBody (..),
                                               HttpMethod (..),
                                               HttpResponse (..), MonadHttp)
import           Network.HTTP.Types           (methodDelete)
import           Network.HTTP.Types.Header    (hContentLength)

-- |
-- Helper function that use custamized Manager
getHttpResponse' :: (HttpResponse a, MonadHttp m) => Proxy a -> Request -> Manager -> m a
getHttpResponse' Proxy r _ = liftIO $ getHttpResponse r =<< fixEmptyStringManager

fixEmptyStringManager :: IO Manager
fixEmptyStringManager = do
  context <- initConnectionContext
  let settings = mkManagerSettingsContext (Just context) def Nothing
  newManager $ settings { managerModifyResponse = fixEmptyString }

-- |
-- if response is no contents, replace "[]".
-- aeson return parse error when response is no content response
fixEmptyString :: Response BodyReader -> IO (Response BodyReader)
fixEmptyString res = do
  reader <- constBodyReader ["[]"]
  let
    contentLength = fromMaybe "0" $ lookup hContentLength (responseHeaders res)
  return $ if contentLength /= "0" then res else res { responseBody = reader }

-- |
-- if want to use Delete HTTP methos with request param, use this type.
-- ref : <https://hackage.haskell.org/package/req-0.3.0/docs/Network-HTTP-Req.html#t:DELETE>
data DELETE2 = DELETE2

instance HttpMethod DELETE2 where
  type AllowsBody DELETE2 = 'CanHaveBody
  httpMethodName Proxy = methodDelete

-- |
-- for resolve ambiguous type
strLength :: String -> Int
strLength = length
