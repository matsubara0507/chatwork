{-# LANGUAGE DeriveGeneric #-}

module ChatWork.Types
    ( Token
    , GetMeResponse(..)
    ) where

import Data.Aeson
import Data.Aeson.Casing
import Data.Text (Text)
import Data.ByteString (ByteString)
import GHC.Generics

type Token = ByteString

data GetMeResponse = GetMeResponse
                   { meAccountId :: Int
                   , meRoomId :: Int
                   , meName :: Text
                   , meChatworkId :: Text
                   , meOrganizationId :: Int
                   , meOrganizationName :: Text
                   , meDepartment :: Text
                   , meTitle :: Text
                   , meUrl :: Text
                   , meIntroduction :: Text
                   , meMail :: Text
                   , meTelOrganization :: Text
                   , meTelExtension :: Text
                   , meTelMobile :: Text
                   , meSkype :: Text
                   , meFacebook :: Text
                   , meTwitter :: Text
                   , meAvatarImageUrl :: Text
                   } deriving (Show, Generic)

instance ToJSON GetMeResponse where
  toJSON = genericToJSON $ aesonDrop (strLength "me") snakeCase
instance FromJSON GetMeResponse where
  parseJSON = genericParseJSON $ aesonDrop (strLength "me") snakeCase

strLength :: String -> Int
strLength = length
