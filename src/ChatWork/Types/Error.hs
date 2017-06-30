{-# LANGUAGE DeriveGeneric #-}

module ChatWork.Types.Error
    ( ChatWorkErrors
    ) where

import           ChatWork.Utils    (strLength)
import           Data.Aeson        (FromJSON (..), ToJSON (..),
                                    genericParseJSON, genericToJSON)
import           Data.Aeson.Casing (aesonDrop, snakeCase)
import           Data.Text         (Text)
import           GHC.Generics      (Generic)

newtype ChatWorkErrors = ChatWorkErrors { getErrors :: [Text] } deriving (Show, Generic)

instance ToJSON ChatWorkErrors where
  toJSON = genericToJSON $ aesonDrop (strLength "get") snakeCase
instance FromJSON ChatWorkErrors where
  parseJSON = genericParseJSON $ aesonDrop (strLength "get") snakeCase
