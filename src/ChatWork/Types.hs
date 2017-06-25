{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE OverlappingInstances #-}

module ChatWork.Types
    ( ChatWorkResponse(..)
    , ToReqParam(..)

    , module ChatWork.Types.Base
    , module ChatWork.Types.Contacts
    , module ChatWork.Types.Error
    , module ChatWork.Types.IncomingRequests
    , module ChatWork.Types.Me
    , module ChatWork.Types.My
    , module ChatWork.Types.Rooms
    ) where

import ChatWork.Types.Base
import ChatWork.Types.Contacts
import ChatWork.Types.Error
import ChatWork.Types.IncomingRequests
import ChatWork.Types.Me
import ChatWork.Types.My
import ChatWork.Types.Rooms

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), Value(..))
import Data.Monoid (Monoid)
import Data.Text (Text, pack)
import GHC.Generics
import Network.HTTP.Req (QueryParam, (=:))

type ChatWorkResponse a = Either ChatWorkErrors a

instance {-# OVERLAPS #-} (FromJSON a) => FromJSON (ChatWorkResponse a) where
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
