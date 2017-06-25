{-# LANGUAGE DeriveGeneric #-}

module ChatWork.Types.IncomingRequests
    ( GetIncomingRequestsResponse
    , PutIncomingRequestsResponse(..)
    ) where

import ChatWork.Types.Base (IncomingRequest)
import ChatWork.Utils (strLength)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Text (Text)
import GHC.Generics (Generic)

type GetIncomingRequestsResponse = [IncomingRequest]

data PutIncomingRequestsResponse = IncomingRequestsResponse
                                 { acceptIncomingRequestAccountId :: Int
                                 , acceptIncomingRequestRoomId :: Int
                                 , acceptIncomingRequestName :: Text
                                 , acceptIncomingRequestChatworkId :: Text
                                 , acceptIncomingRequestOrganizationId :: Int
                                 , acceptIncomingRequestOrganizationName :: Text
                                 , acceptIncomingRequestDepartment :: Text
                                 , acceptIncomingRequestAvatarImageUrl :: Text
                                 } deriving (Show, Generic)

instance ToJSON PutIncomingRequestsResponse where
  toJSON = genericToJSON $ aesonDrop (strLength "acceptIncomingRequest") snakeCase
instance FromJSON PutIncomingRequestsResponse where
  parseJSON = genericParseJSON $ aesonDrop (strLength "acceptIncomingRequest") snakeCase
