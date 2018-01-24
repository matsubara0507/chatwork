{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChatWork.Types (
      JsonResponse'
    , jsonResponse'
    -- * type synonym of Response Json
    , ChatWorkResponse
    -- * Helper type class for constructing Request paramater
    , ToReqParam(..)

    , module Types
    ) where

import           ChatWork.Types.Base             as Types
import           ChatWork.Types.Contacts         as Types
import           ChatWork.Types.Error            as Types
import           ChatWork.Types.IncomingRequests as Types
import           ChatWork.Types.Me               as Types
import           ChatWork.Types.My               as Types
import           ChatWork.Types.Rooms            as Types

import           ChatWork.Types.Base             (IconPreset, TaskStatus)
import           ChatWork.Types.Error            (ChatWorkErrors)
import           Control.Applicative             ((<|>))
import           Control.Exception               (throwIO)
import           Data.Aeson                      (FromJSON (..), eitherDecode)
import qualified Data.ByteString.Lazy            as BL
import           Data.Monoid                     (Monoid)
import           Data.Proxy                      (Proxy (..))
import           Data.Text                       (Text, pack)
import qualified Network.HTTP.Client             as L
import           Network.HTTP.Req                (HttpException (..),
                                                  HttpResponse (..), QueryParam,
                                                  (=:))

newtype JsonResponse' a = JsonResponse' (L.Response a)

instance FromJSON a => HttpResponse (JsonResponse' a) where
  type HttpResponseBody (JsonResponse' a) = a
  toVanillaResponse (JsonResponse' r) = r
  getHttpResponse r = do
    chunks <- L.brConsume (L.responseBody r)
    let
      body = if null chunks then "[]" else BL.fromChunks chunks
    case eitherDecode body of
      Left  e -> throwIO (JsonHttpException e)
      Right x -> return $ JsonResponse' (x <$ r)

jsonResponse' :: Proxy (JsonResponse' a)
jsonResponse' = Proxy

-- |
-- Wrapper type synonym of 'JsonResponse' and 'ChatWorkErrors'
type ChatWorkResponse a = JsonResponse' (Either ChatWorkErrors a)

instance {-# OVERLAPS #-} (FromJSON a) => FromJSON (Either ChatWorkErrors a) where
  parseJSON v = (Left <$> parseJSON v) <|> (Right <$> parseJSON v)

-- |
-- Helper Type Class of 'QueryParam'
-- use to construct request parameter from param type, e.g. 'CreateRoomParams'

class ToReqParam a where
  toReqParam :: (QueryParam param, Monoid param) => Text -> a -> param

instance ToReqParam Int where
  toReqParam = (=:)

instance ToReqParam Text where
  toReqParam = (=:)

instance ToReqParam a => ToReqParam (Maybe a) where
  toReqParam = maybe mempty . toReqParam

instance Show a => ToReqParam [a] where
  toReqParam name = toReqParam name . foldl1 (\acc txt -> mconcat [acc, ",", txt]) . fmap (pack . show)

instance ToReqParam IconPreset where
  toReqParam name = toReqParam name . pack . show

instance ToReqParam TaskStatus where
  toReqParam name = toReqParam name . pack . show
