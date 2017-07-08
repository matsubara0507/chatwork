{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ChatWork.Types.Base
    ( Room(..)
    , Account(..)
    , IconPreset(..)
    , TaskStatus (..)
    , AccountId
    ) where

import           ChatWork.Utils    (strLength)
import           Data.Aeson        (FromJSON (..), ToJSON (..),
                                    genericParseJSON, genericToJSON)
import           Data.Aeson.Casing (aesonDrop, snakeCase)
import           Data.Either       (partitionEithers)
import           Data.Text         (Text, pack, split)
import           GHC.Generics      (Generic)
import           Web.HttpApiData

data Room = Room
          { roomToRoomId   :: Int
          , roomToName     :: Text
          , roomToIconPath :: Text
          } deriving (Eq, Show, Generic)

instance ToJSON Room where
  toJSON = genericToJSON $ aesonDrop (strLength "roomTo") snakeCase
instance FromJSON Room where
  parseJSON = genericParseJSON $ aesonDrop (strLength "roomTo") snakeCase

data Account = Account
             { accountToAccountId      :: Int
             , accountToName           :: Text
             , accountToAvatarImageUrl :: Text
             } deriving (Eq, Show, Generic)

instance ToJSON Account where
  toJSON = genericToJSON $ aesonDrop (strLength "accountTo") snakeCase
instance FromJSON Account where
  parseJSON = genericParseJSON $ aesonDrop (strLength "accountTo") snakeCase

-- |
-- use create new room
-- see : <http://developer.chatwork.com/ja/endpoint_rooms.html#POST-rooms>
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
                deriving (Bounded, Enum, Eq)

instance Show IconPreset where
  show Group    = "group"
  show Check    = "check"
  show Document = "document"
  show Meeting  = "meeting"
  show Event    = "event"
  show Project  = "project"
  show Business = "business"
  show Study    = "study"
  show Security = "security"
  show Star     = "star"
  show Idea     = "idea"
  show Heart    = "heart"
  show Magcup   = "magcup"
  show Beer     = "beer"
  show Music    = "music"
  show Sports   = "sports"
  show Travel   = "travel"

instance ToHttpApiData IconPreset where
  toUrlPiece = pack . show

instance FromHttpApiData IconPreset where
  parseUrlPiece = parseBoundedUrlPiece

-- |
-- use get tasks on room
-- see: <http://developer.chatwork.com/ja/endpoint_rooms.html#GET-rooms-room_id-tasks>
data TaskStatus = Open | Done deriving (Bounded, Enum, Eq)

instance Show TaskStatus where
  show Open = "open"
  show Done = "done"

instance ToHttpApiData TaskStatus where
  toUrlPiece = pack . show

instance FromHttpApiData TaskStatus where
  parseUrlPiece = parseBoundedUrlPiece

-- |
-- use get files on room
-- see: <http://developer.chatwork.com/ja/endpoint_rooms.html#GET-rooms-room_id-files>
type AccountId = Int

instance (FromHttpApiData a) => FromHttpApiData [a] where
  parseUrlPiece txt = if null es then Right xs else Left "abc"
    where
      (es, xs) = partitionEithers . fmap parseUrlPiece $ split (== ',') txt
