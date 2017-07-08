{-# LANGUAGE OverloadedStrings #-}

module ChatWork.Test.Types
    ( MessageParams (..)
    ) where

import           ChatWork.Types
import           Web.FormUrlEncoded

newtype MessageParams = MessageParams MessageBody deriving (Show)

instance FromForm MessageParams where
  fromForm f = MessageParams <$> parseUnique "body" f
