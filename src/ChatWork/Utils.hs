{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module ChatWork.Utils (
    -- * DELETE HTTP method with paramater
      DELETE2(..)
    -- * help to make 'FromJSON' instance
    , strLength
    ) where

import           Data.Proxy         (Proxy (..))
import           Network.HTTP.Req   (AllowsBody (..), CanHaveBody (..),
                                     HttpMethod (..))
import           Network.HTTP.Types (methodDelete)

-- |
-- if want to use Delete HTTP methos with request param, use this type.
-- ref : <https://hackage.haskell.org/package/req-0.3.0/docs/Network-HTTP-Req.html#t:DELETE>
data DELETE2 = DELETE2

instance HttpMethod DELETE2 where
  type AllowsBody DELETE2 = 'CanHaveBody
  httpMethodName Proxy = methodDelete

-- |
-- for resolve ambiguous type
strLength :: String -> Int
strLength = length
