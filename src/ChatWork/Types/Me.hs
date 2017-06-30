{-# LANGUAGE DeriveGeneric #-}

module ChatWork.Types.Me
    ( Me(..)
    ) where

import ChatWork.Utils (strLength)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Text (Text)
import GHC.Generics (Generic)

data Me = Me
         { meToAccountId :: Int
         , meToRoomId :: Int
         , meToName :: Text
         , meToChatworkId :: Text
         , meToOrganizationId :: Int
         , meToOrganizationName :: Text
         , meToDepartment :: Text
         , meToTitle :: Text
         , meToUrl :: Text
         , meToIntroduction :: Text
         , meToMail :: Text
         , meToTelOrganization :: Text
         , meToTelExtension :: Text
         , meToTelMobile :: Text
         , meToSkype :: Text
         , meToFacebook :: Text
         , meToTwitter :: Text
         , meToAvatarImageUrl :: Text
         } deriving (Show, Generic)

instance ToJSON Me where
  toJSON = genericToJSON $ aesonDrop (strLength "meTo") snakeCase
instance FromJSON Me where
  parseJSON = genericParseJSON $ aesonDrop (strLength "meTo") snakeCase
