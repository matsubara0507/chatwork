{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE OverlappingInstances #-}

module ChatWork.Types
    ( ChatWorkResponse(..)
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
import           Data.Aeson                      (FromJSON (..), Value (..))
import           Data.Monoid                     (Monoid)
import           Data.Text                       (Text, pack)
import           GHC.Generics
import           Network.HTTP.Req                (JsonResponse, QueryParam,
                                                  (=:))

type ChatWorkResponse a = JsonResponse (Either ChatWorkErrors a)

instance {-# OVERLAPS #-} (FromJSON a) => FromJSON (Either ChatWorkErrors a) where
  parseJSON v = ((Left <$> parseJSON v) <|> (Right <$> parseJSON v))

class ToReqParam a where
  toReqParam :: (QueryParam param, Monoid param) => Text -> a -> param

instance ToReqParam Int where
  toReqParam = (=:)

instance ToReqParam Text where
  toReqParam = (=:)

instance ToReqParam a => ToReqParam (Maybe a) where
  toReqParam = maybe mempty . toReqParam

instance Show a => ToReqParam [a] where
  toReqParam name = toReqParam name . foldl (\acc a -> mconcat [acc, ",", pack $ show a]) ""

instance ToReqParam IconPreset where
  toReqParam name = toReqParam name . pack . show

instance ToReqParam TaskStatus where
  toReqParam name = toReqParam name . pack . show
