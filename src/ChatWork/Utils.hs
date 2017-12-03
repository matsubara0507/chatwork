{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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

import           Control.Exception            (bracket, handle, mask_, throwIO,
                                               try)
import           Control.Monad                ((>=>))
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Retry                (retrying)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import           Data.Default.Class           (def)
import           Data.IORef                   (modifyIORef', newIORef,
                                               readIORef, writeIORef)
import           Data.List                    (lookup)
import           Data.Maybe                   (fromMaybe)
import           Data.Proxy                   (Proxy (..))
import           Network.Connection           (initConnectionContext)
import           Network.HTTP.Client          (BodyReader, Manager,
                                               ManagerSettings (..), Request,
                                               Response (..))
import qualified Network.HTTP.Client          as L
import qualified Network.HTTP.Client.Internal as LI
import qualified Network.HTTP.Client.TLS      as L
import           Network.HTTP.Req             (AllowsBody (..),
                                               CanHaveBody (..),
                                               HttpConfig (..),
                                               HttpException (..),
                                               HttpMethod (..),
                                               HttpResponse (..),
                                               MonadHttp (..), getHttpConfig)
import           Network.HTTP.Types           (methodDelete)
import           Network.HTTP.Types.Header    (hContentLength)

-- |
-- Helper function that use custamized Manager
getHttpResponse' :: (HttpResponse a, MonadHttp m) => Proxy a -> Request -> Manager -> m a
getHttpResponse' Proxy request _ = do
  HttpConfig {..}  <- getHttpConfig
  let wrapVanilla = handle (throwIO . VanillaHttpException)
      wrapExc     = handle (throwIO . LI.toHttpException request)
      withRRef    = bracket
        (newIORef Nothing)
        (readIORef >=> mapM_ L.responseClose)
  (liftIO . try . wrapVanilla . wrapExc) (withRRef $ \rref -> do
    manager <- fixEmptyStringManager
    let openResponse = mask_ $ do
          r  <- readIORef rref
          mapM_ L.responseClose r
          r' <- L.responseOpen request manager
          writeIORef rref (Just r')
          return r'
    r <- retrying
      httpConfigRetryPolicy
      (\st r -> return $ httpConfigRetryJudge st r)
      (const openResponse)
    (preview, r') <- grabPreview bodyPreviewLength r
    mapM_ LI.throwHttp (httpConfigCheckResponse request r' preview)
    getHttpResponse r')
    >>= either handleHttpException return

fixEmptyStringManager :: IO Manager
fixEmptyStringManager = do
  context <- initConnectionContext
  let settings = L.mkManagerSettingsContext (Just context) def Nothing
  L.newManager $ settings { managerModifyResponse = fixEmptyString }

-- |
-- if response is no contents, replace "[]".
-- aeson return parse error when response is no content response
fixEmptyString :: Response BodyReader -> IO (Response BodyReader)
fixEmptyString res = do
  reader <- LI.constBodyReader ["[]"]
  let
    contentLength = fromMaybe "0" $ lookup hContentLength (responseHeaders res)
  return $ if contentLength /= "0" then res else res { responseBody = reader }

----------------------------------------------------------------------------
-- Helpers for response interpretations

grabPreview :: Int -> L.Response L.BodyReader -> IO (ByteString, L.Response L.BodyReader)
grabPreview nbytes r = do
  let br = L.responseBody r
  (target, leftover, done) <- brReadN br nbytes
  nref <- newIORef (0 :: Int)
  let br' = do
        n <- readIORef nref
        let incn = modifyIORef' nref (+ 1)
        case n of
          0 -> do
            incn
            if B.null target
              then br'
              else return target
          1 -> do
            incn
            if B.null leftover
              then br'
              else return leftover
          _ ->
            if done
              then return B.empty
              else br
  return (target, r { L.responseBody = br' })

brReadN :: L.BodyReader -> Int -> IO (ByteString, ByteString, Bool)
brReadN br n = go 0 id id
  where
    go !tlen t l = do
      chunk <- br
      if B.null chunk
        then return (r t, r l, True)
        else do
          let (target, leftover) = B.splitAt (n - tlen) chunk
              tlen'              = B.length target
              t'                 = t . (target:)
              l'                 = l . (leftover:)
          if tlen + tlen' < n
            then go (tlen + tlen') t' l'
            else return (r t', r l', False)
    r f = B.concat (f [])

bodyPreviewLength :: Num a => a
bodyPreviewLength = 1024

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
