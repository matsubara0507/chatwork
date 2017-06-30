{-# LANGUAGE DeriveGeneric #-}

module ChatWork.Types.Contacts
    ( Contacts
    , Contact(..)
    ) where

import           ChatWork.Utils    (strLength)
import           Data.Aeson        (FromJSON (..), ToJSON (..),
                                    genericParseJSON, genericToJSON)
import           Data.Aeson.Casing (aesonDrop, snakeCase)
import           Data.Text         (Text)
import           GHC.Generics      (Generic)

type Contacts = [Contact]

data Contact = Contact
             { contactToAccountId        :: Int
             , contactToRoomId           :: Int
             , contactToName             :: Text
             , contactToChatworkId       :: Text
             , contactToOrganizationId   :: Int
             , contactToOrganizationName :: Text
             , contactToDepartment       :: Text
             , contactToAvatarImageUrl   :: Text
             } deriving (Show, Generic)

instance ToJSON Contact where
  toJSON = genericToJSON $ aesonDrop (strLength "contactTo") snakeCase
instance FromJSON Contact where
  parseJSON = genericParseJSON $ aesonDrop (strLength "contactTo") snakeCase
