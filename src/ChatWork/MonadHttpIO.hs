-- |
-- This module is that define MonadHttp type class instance of IO.
-- if you want other definition, don't import this module.

module ChatWork.MonadHttpIO where

import           Control.Exception (throwIO)
import           Network.HTTP.Req

instance MonadHttp IO where
  handleHttpException = throwIO
