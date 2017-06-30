{-# LANGUAGE DeriveGeneric #-}

module ChatWork.Types.Base
    ( Room(..)
    , Account(..)
    , IconPreset(..)
    , TaskStatus
    , AccountId
    ) where

import ChatWork.Utils (strLength)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Text (Text)
import GHC.Generics (Generic)

data Room = Room
          { roomToRoomId :: Int
          , roomToName :: Text
          , roomToIconPath :: Text
          } deriving (Show, Generic)

instance ToJSON Room where
  toJSON = genericToJSON $ aesonDrop (strLength "roomTo") snakeCase
instance FromJSON Room where
  parseJSON = genericParseJSON $ aesonDrop (strLength "roomTo") snakeCase

data Account = Account
             { accountToAccountId :: Int
             , accountToName :: Text
             , accountToAvatarImageUrl :: Text
             } deriving (Show, Generic)

instance ToJSON Account where
  toJSON = genericToJSON $ aesonDrop (strLength "accountTo") snakeCase
instance FromJSON Account where
  parseJSON = genericParseJSON $ aesonDrop (strLength "accountTo") snakeCase

data IconPreset = Group
                | Check
                | Document
                | Meeting
                | Event
                | Project
                | Business
                | Study
                | Security
                | Star
                | Idea
                | Heart
                | Magcup
                | Beer
                | Music
                | Sports
                | Travel
                deriving (Eq)

instance Show IconPreset where
  show Group = "group"
  show Check = "check"
  show Document = "document"
  show Meeting = "meeting"
  show Event = "event"
  show Project = "project"
  show Business = "business"
  show Study = "study"
  show Security = "security"
  show Star = "star"
  show Idea = "idea"
  show Heart = "heart"
  show Magcup = "magcup"
  show Beer = "beer"
  show Music = "music"
  show Sports = "sports"
  show Travel = "travel"

data TaskStatus = Open | Done

instance Show TaskStatus where
  show Open = "open"
  show Done = "done"

type AccountId = Int
