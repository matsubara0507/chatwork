{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Types
    ( ToReqParam(..)

    , module ChatWork.Types.Base
    , module ChatWork.Types.Contacts
    , module ChatWork.Types.IncomingRequests
    , module ChatWork.Types.Me
    , module ChatWork.Types.My
    , module ChatWork.Types.Rooms
    ) where

import ChatWork.Types.Base
import ChatWork.Types.Contacts
import ChatWork.Types.IncomingRequests
import ChatWork.Types.Me
import ChatWork.Types.My
import ChatWork.Types.Rooms

-- import Data.Aeson.Types

import Data.Monoid (Monoid)
import Data.Text (Text, pack)
import GHC.Generics
import Network.HTTP.Req (QueryParam, (=:))

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
