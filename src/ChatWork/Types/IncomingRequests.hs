{-# LANGUAGE DeriveGeneric #-}

module ChatWork.Types.IncomingRequests
    ( IncomingRequests
    , IncomingRequest(..)
    , AcceptedIncomingRequest(..)
    ) where

import           ChatWork.Utils    (strLength)
import           Data.Aeson        (FromJSON (..), ToJSON (..),
                                    genericParseJSON, genericToJSON)
import           Data.Aeson.Casing (aesonDrop, snakeCase)
import           Data.Text         (Text)
import           GHC.Generics      (Generic)

type IncomingRequests = [IncomingRequest]

data IncomingRequest = IncomingRequest
                     { incomingRequestToRequestId        :: Int
                     , incomingRequestToAccountId        :: Int
                     , incomingRequestToMessage          :: Text
                     , incomingRequestToName             :: Text
                     , incomingRequestToChatworkId       :: Text
                     , incomingRequestToOrganizationId   :: Int
                     , incomingRequestToOrganizationName :: Text
                     , incomingRequestToDepartment       :: Text
                     , incomingRequestToAvatarImageUrl   :: Text
                     } deriving (Eq, Show, Generic)

instance ToJSON IncomingRequest where
  toJSON = genericToJSON $ aesonDrop (strLength "incomingRequestTo") snakeCase
instance FromJSON IncomingRequest where
  parseJSON = genericParseJSON $ aesonDrop (strLength "incomingRequestTo") snakeCase

data AcceptedIncomingRequest = AcceptedIncomingRequest
                             { acceptedIncomingRequestToAccountId        :: Int
                             , acceptedIncomingRequestToRoomId           :: Int
                             , acceptedIncomingRequestToName             :: Text
                             , acceptedIncomingRequestToChatworkId       :: Text
                             , acceptedIncomingRequestToOrganizationId   :: Int
                             , acceptedIncomingRequestToOrganizationName :: Text
                             , acceptedIncomingRequestToDepartment       :: Text
                             , acceptedIncomingRequestToAvatarImageUrl   :: Text
                             } deriving (Eq, Show, Generic)

instance ToJSON AcceptedIncomingRequest where
  toJSON = genericToJSON $ aesonDrop (strLength "acceptedIncomingRequestTo") snakeCase
instance FromJSON AcceptedIncomingRequest where
  parseJSON = genericParseJSON $ aesonDrop (strLength "acceptedIncomingRequestTo") snakeCase
